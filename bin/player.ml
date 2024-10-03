open Matrix

module L = List

module C = Common
module Cr = Creature
module P = Position
module R = Random_
module S = State
module SL = StateLevels
module SP = StatePlayer

let goldMax = Item.getPriceTrade Item.scepterOfYorel

type fDir = P.t -> P.t

type actions =
    | Absorb
    | BlindUnblind
    | Close of fDir
    | Drop of C.selectionItem list
    | MoveDir of fDir
    | Pickup of C.selectionItem list
    | Quaff of C.selectionItem
    | Read of C.selectionItem
    | Search
    | Throw of C.selectionItem * P.dir
    | Wield of C.selectionItem
    | Zap of C.selectionItem * P.dir

let absorb (state : S.t) =
    let player = state.player in
    let p = player.pos in

    let m = SL.map state in
    let t = Matrix.get m p in

    let corpses =
        t.items
        |> L.filter_map
            (function
            | Item.{ t = Corpse c; _ } -> Some c
            | _ -> None
            )
    in

    match corpses with
    | [] -> S.msgAdd state "There's nothing to absorb here."; state
    | c::_ ->
        let state = match Item.corpseFreshness c state.turns with
        | Item.Fresh ->
            ( match c.name with
                | "floating eye" when not (Cr.isTelepath player.attributes) ->
                    S.msgAdd state "You feel a strange mental acuity.";
                    UpdatePlayer.addAttribute Telepathic state
                    (* TODO Better for corpses to have attributes *)
                | _ ->
                    let bonus = R.rn 1 (c.weight / 100 |> max 1) in
                    S.msgAdd state "That felt good.";
                    UpdatePlayer.addHp bonus state
            )
        | Unfresh ->
            S.msgAdd state "That corpse didn't feel fresh.";
            state
        | Unhealthy ->
            S.msgAdd state "That felt unhealthy.";
            let bonus = R.rn 1 (c.weight / 50 |> max 5) in
            UpdatePlayer.addHp (-bonus) state

        | Tainted -> match R.rn 0 2 with
            | 0 ->
                S.msgAdd state "That corpse has acquired a deadly taint...";
                UpdatePlayer.addHp (-player.hpMax) state
            | _ ->
                S.msgAdd state "That corpse was far too old!";
                let bonus = R.rn 0 (c.weight / 25 |> max 10) in
                UpdatePlayer.addHp (-bonus) state
        in
        let toRemove =
            Item.
            { t = Corpse c
            ; stats = { stack = NonStackable }
            }
            (* ^TODO do we really need to rebuild the item? *)
        in
        let t = { t with items = Items.remove t.items toRemove (C.Count 1) } in
        let m = Matrix.set t p m in
        SL.setMap m state

let blindUnblind (state : S.t) =
    let player = state.player in
    let attributes = player.attributes in

    let attributes = match C.contains attributes Blind with
        | true ->
            S.msgAdd state "You remove the rag from your eyes.";
            C.listRemove Cr.Blind attributes
        | false ->
            S.msgAdd state "You wear a rag to cover your eyes.";
            Blind::attributes
    in
    let player = { player with attributes } in
    { state with player }

let attackMelee p (c : Creature.t) (state : S.t) =
    let acTarget = Cr.getAc c in
    let hitThreshold = Attack.getHitThreshold acTarget state.player.level in

    let nameTarget = match Sight.playerCanSee state p with
        | true -> C.sf "the %s" c.info.name
        | false -> "it"
    in

    match R.rollAttackLanded hitThreshold 0 with
    | Miss ->
        S.msgAdd state (C.sf "You miss %s." nameTarget);
        state
    | MissBarely ->
        S.msgAdd state (C.sf "You just miss %s." nameTarget);
        state
    | Hit ->
        S.msgAdd state (C.sf "You hit %s." nameTarget);
        let state = Ai.doCreaturePassive c state in
        let sp = state.player in
        let damage =
            ( match sp.weaponWielded with
            | None ->
                let showMessage = R.oneIn 26 in
                ( if showMessage then
                    S.msgAdd state "Your bare fists don't seem to do much..."
                );
                ( if showMessage && L.exists Item.isWeapon sp.inventory then
                    S.msgAdd state "It would be wise to (w)ield a weapon.";
                );
                R.rn 1 2 (* bare-handed *)
            | Some w ->
                ( match w.t with
                | Weapon _ -> R.roll (Item.getWeaponDamage w)
                | _ -> R.rn 1 2
                )
            )
            |> Attack.reduceDamage acTarget
        in
        UpdateCreature.addHp ~sourceIsPlayer:true (-damage) p c state

let rec move mf (state : S.t) =
    let p = state.player.pos in
    let m = SL.map state in
    let pn = mf p in
    if not (Map.isInMap pn) then state else

    match Matrix.get m pn with
    | { t = Door (Closed, ori); _ } as tile ->
        (* TODO make chance based on stats *)
        if R.oneIn 3 then
            let _ = S.msgAdd state "The door resists!" in
            state
        else
            let _  = S.msgAdd state "You open the door." in
            let t = Map.Door (Open, ori) in
            let tn = Matrix.set { tile with t } pn m in
            let state = UpdatePlayer.knowledgeMapTileType t pn state in
            SL.setMap tn state

    | { occupant = Some (Creature c); _ } when not (Sight.playerCanSee state pn) ->
        attackMelee pn c state

    | { occupant = Some (Creature c); _ } when Creature.isHostile c ->
        attackMelee pn c state

    | { occupant = Some (Creature c); _ } when Creature.isDocile c ->
        S.msgAdd state (C.sf "The %s is there." c.info.name);
        state

    | { occupant = Some (Creature c); _ } when Creature.isPeaceful c ->
        S.msgAdd state (C.sf "The %s is there." c.info.name);
        state

    | { occupant = Some Boulder; _ } as t ->
        let pbNew = mf pn in
        let state = UpdatePlayer.knowledgeMapTileOccupant t.occupant pn state in
        if not (Map.isInMap pbNew) then (S.msgAdd state "The boulder won't budge!"; state) else
        let behindBoulder = Matrix.get m pbNew in
        if Map.isTileTypeWalkable behindBoulder |> not then (S.msgAdd state "The boulder won't budge."; state) else
        ( match behindBoulder with
            | { occupant = Some Boulder; _ } -> S.msgAdd state "There's something blocking the boulder!"; state
            | { occupant = Some _; _ } -> S.msgAdd state "There's something alive behind the boulder!"; state
                (* ^TODO icon for the above *)
            | _ ->
                let behind' = { behindBoulder with occupant = Some Boulder } in
                let state = UpdatePlayer.knowledgeMapTileOccupant behind'.occupant pbNew state in
                let t' = { t with occupant = None } in
                let m' =
                    Matrix.set behind' pbNew m
                    |> Matrix.set t' pn
                in
                S.msgAdd state "With great effort you push the boulder.";
                move mf (SL.setMap m' state)
        )

    | tNew ->
        if not (Map.isTileTypeWalkable tNew) then
            (* Bumping into walls *)
            match Cr.isBlind state.player.attributes with
            | true -> UpdatePlayer.knowledgeMapTileType tNew.t pn state
            | false -> state
        else
        let tile = Matrix.get m p in
        let player = { state.player with pos = pn } in
        let state' = { state with player } in
        let m' =
            (* displace pet, if present *)
            Matrix.set { tile with occupant = tNew.occupant } p m
            |> Matrix.set { tNew with occupant = Some Player } pn
        in
        let _ = if Map.isStairs tNew then S.msgAdd state "There are stairs here." in
        ( if not (List.is_empty tNew.items) then
            if List.length tNew.items > Config.itemsDisplayedMax then
                S.msgAdd state "Here are many items."
            else
                let _ = List.iter
                    ( fun i ->
                        let price = if SP.isInShop state' then C.sf "(%i gold)" (Item.getPriceShop i) else "" in
                        S.msgAdd state (C.sf "%s %s" (Item.nameDisplay i) price)
                    )
                    tNew.items
                in
                S.msgAdd state "Items here:"
        else
            ()
        );

        { (SL.setMap m' state') with player }

let search (state : S.t) =
    (* TODO base search success on stats *)
    let currentLevel = SL.map state in
    let hiddenTerrainAround =
        Map.posAround state.player.pos
        |> List.filter (fun pa -> Matrix.get currentLevel pa |> Map.isTerrainHidden)
    in
    let terrain', state = List.fold_left
        ( fun (m, state) p ->
            if R.nToOne 2 then
                (* ^TODO base on stats *)
                m, state
            else
                let (current : Map.tile) = Matrix.get m p in
                let tt' = match current.t with
                    | Door (Hidden, ori) -> S.msgAdd state "You find a hidden door!"; Map.Door (Closed, ori)
                    | Hallway HallHidden -> S.msgAdd state "You find a hidden hallway!"; Hallway HallRegular
                    | _ -> assert false
                in
                let m = Matrix.set { current with t = tt' } p m in
                let state = UpdatePlayer.knowledgeMapTileType tt' p state in
                m, state
        ) (currentLevel, state) hiddenTerrainAround
    in

    SL.setMap terrain' state

let quaff (si : C.selectionItem) (state : S.t) =
    let sp = state.player in
    let item = List.nth sp.inventory si.iIndex in
    match item.t with
        | Comestible _ -> S.msgAdd state "Slow down! Chew your food!"; state
        | Container _ -> S.msgAdd state "What a silly thing to quaff!"; state
        | Corpse _ -> S.msgAdd state "What a silly thing to quaff!"; state
        | Gold -> S.msgAdd state "You were unable to swallow the gold piece."; state
        | Rock -> S.msgAdd state "You were unable to swallow the rock."; state
        | Scroll _ -> S.msgAdd state "This scroll is quite solid. Quite difficult to drink..."; state
        | Weapon _ -> S.msgAdd state "You change your mind about swallowing your weapon."; state
        | Wand _ -> S.msgAdd state "You change your mind about swallowing your wand."; state
        | Potion p ->
            let inventory = Items.remove sp.inventory item (C.Count 1) in
            let player = { sp with inventory } in
            let state = { state with player } in
            match p with
            | Healing -> S.msgAdd state "You feel better."; UpdatePlayer.addHp (8 + (R.roll {sides=4; rolls=4})) state
            | HealingExtra -> S.msgAdd state "You feel much better."; UpdatePlayer.addHp (16 + (R.roll {sides=4; rolls=12})) state
            | HealingFull ->
                S.msgAdd state "Thank you kindly for freeing me!";
                S.msgAdd state "You feel completely healed.";
                UpdatePlayer.addHp(sp.hpMax) state
            | Sickness -> S.msgAdd state "This tastes like poison."; UpdatePlayer.addHp (R.rn (-100) (-10)) state

let findFairy (state : S.t) =
    let sp = state.player in
    let fairies = L.mapi
        ( fun ix (i : Item.t) -> match i.t with
            | Item.Potion HealingFull -> Some ix
            | _ -> None
        )
        sp.inventory
        |> L.filter_map C.id
    in
    match fairies with
    | [] -> None
    | iIndex::_ ->
        Some
        C.
        { iIndex
        ; howMany = Count 1
        ; name = ""
        ; selected = true
        ; letter = ' '
        ; selectionMax = 1
        }

let maybeWarnHealth (state : S.t) msg =
    let warnHealthTimeout = 100 in
    let sp = state.player in

    if sp.turnHealthWarned + warnHealthTimeout > state.turns then state else

    let _ = S.msgAdd state msg in
    let player = { sp with turnHealthWarned = state.turns } in

    { state with player }

let checkHp (state : S.t) =
    let sp = state.player in
    match sp.hp with
    | hp when hp <= 0 ->
        ( match findFairy state with
        | None ->
            let _ = S.msgAdd state "You die..." in
            { state with mode = Dead }
        | Some f ->
            let player = { sp with timesKilled = sp.timesKilled + 1 } in
            let state = { state with player } in
            let _ = S.msgAdd state "You die. But you don't really die..." in
            (quaff f state)
        )

    | hp when hp = 1 ->
        maybeWarnHealth state "You are about to die."

    | hp when hp * 10 < sp.hpMax ->
        maybeWarnHealth state "You feel your life force running out..."
        (* ^TODO This message can be seen upon health increase *)

    | _ ->
        state

let read (si : C.selectionItem) (state : S.t) =
    let sp = state.player in
    let item = List.nth sp.inventory si.iIndex in
    match item.t with
        | Comestible _ -> S.msgAdd state "What a silly thing to read!"; state
        | Container _ -> S.msgAdd state "What a silly thing to read!"; state
        | Corpse _ -> S.msgAdd state "What a silly thing to read!"; state
        | Gold -> S.msgAdd state "The gold is shiny!"; state
        | Potion _ -> S.msgAdd state "This potion is unlabeled."; state
        | Rock -> S.msgAdd state "This rock is not a tree."; state
        | Weapon _ -> S.msgAdd state "There's nothing to read on this weapon."; state
        | Wand _ -> S.msgAdd state "This is indeed a wand."; state
        | Scroll s ->
            let inventory = Items.remove sp.inventory item (C.Count 1) in
            let player = { sp with inventory } in
            let state = { state with player } in
            match s with
            | CreateMonster -> S.msgAdd state "The area feels more dangerous!"; Ai.spawnCreatures ~preferNear:(Near sp.pos) ~room:None state
            | MagicMapping ->
                ( match (SL.level state).level_t with
                | Dungeon
                | Garden _ ->
                    S.msgAdd state "An image coalesces in your mind.";
                    S.setKnowledgeCurrentMap (SL.map state) state (* TODO remove item positions *)
                | Final ->
                    S.msgAdd state "The scroll shudders, then crumbles to dust.";
                    state
                )
            | Teleport ->
                let pp = sp.pos in
                let m = SL.map state in
                let spawnPositions =
                    Map.allMapPositions
                    |> L.filter (fun p -> Ai.canSpawnAt ~forbidPos:None m p)
                in
                match spawnPositions with
                | [] ->
                    S.msgAdd state "You feel surrounded.";
                    state
                | _ ->
                    S.msgAdd state "Your position feels more uncertain.";
                    let pNew = R.item spawnPositions in
                    let mf = P.diff pp pNew |> P.add in
                    move mf state

let wield (si : C.selectionItem) (state : S.t) =
    let sp = state.player in
    let item, inventory = Items.splitIndex sp.inventory si.iIndex (C.Count 1) in
    let weaponWielded = Some item in
    S.msgAdd state (C.sf "You wield %s." (Item.nameDisplay item));

    let unwielded = match sp.weaponWielded with
        | None -> []
        | Some w -> [w]
    in

    let inventory = Items.concat unwielded inventory in
    let player =
        { sp with weaponWielded
        ; inventory
        }

    in
    { state with player }

let zap (si : C.selectionItem) dir (state : S.t) =
    let sp = state.player in
    let item = List.nth sp.inventory si.iIndex in
    let item = match item.t with
        | Item.Wand w ->
            let t = Item.Wand { w with charges = max 0 (w.charges - 1) } in
            { item with t }
        | _ -> assert false
    in
    let inventory = C.listSet si.iIndex item sp.inventory in
    let player = { sp with inventory } in
    let state = { state with player } in

    match item.t with
        | Wand w ->
            if w.charges <= 0 then
                let _ = S.msgAdd state "Nothing happens." in
                state
            else
            let range = R.rn 20 26 in
            ( match w.wand_t with
            | Dig ->
                ( match (SL.level state).level_t with
                | Dungeon
                | Garden _ ->
                    S.msgAdd state "The dungeon seems less solid for a moment.";
                    UpdateMap.dig sp.pos dir range state
                | Final ->
                    S.msgAdd state "The wand gets very hot, but nothing else happens.";
                    state
                )
            | Fire ->
                S.msgAdd state "A column of fire erupts from your wand.";
                Attack.castRay Hit.Fire sp.pos dir range { rolls = 6; sides = 6 } state
            | MagicMissile ->
                S.msgAdd state "A hail of particles shoots from your wand.";
                Attack.castRay Hit.Physical sp.pos dir range { rolls = 2; sides = 6 } state
            | Striking ->
                S.msgAdd state "Your wand emits a loud burst.";
                Attack.castRay Hit.Sonic sp.pos dir range { rolls = 1; sides = 6 } state
            )
        | _ -> S.msgAdd state "can't zap that."; state

let close fDir (state : S.t) =
    let sp = state.player in
    let pClose = fDir sp.pos in

    if not (Map.isInMap pClose) then
        let _ = S.msgAdd state "There's nothing to close there." in
        state
    else

    let m = SL.map state in
    match Matrix.get m pClose with
    | { t = Door (Open, ori); _ } as tile ->
        ( match tile.occupant, tile.items with
        | Some _, _ | _, _::_ ->
            let _ = S.msgAdd state "There's something in the way." in
            state
        | None, [] ->
            if R.oneIn 5 then
                let _ = S.msgAdd state "The door resists!" in
                state
            else
                let t = Map.Door (Closed, ori) in
                let tNew = { tile with t } in
                let _ = S.msgAdd state "You close the door." in
                let m = Matrix.set tNew pClose m in
                let state = UpdatePlayer.knowledgeMapTileType t pClose state in
                SL.setMap m state
            )

    | _ -> S.msgAdd state "There's nothing to close there."; state


let drop sl (state : S.t) =
    let sp = state.player in
    let m = SL.map state in
    let t = Matrix.get m sp.pos in

    let iDropped, iRemain = Items.takeSelection sp.inventory sl in

    let valueTrade items =
        L.map Item.getPriceTrade items
        |> L.fold_left (+) 0
    in
    (* TODO allow dropping gold *)

    let inventory, dropped, gold = match SP.isInShop state with
        | true ->
            ( match valueTrade iDropped with
            | _ when L.exists Item.isCorpse iDropped ->
                let _ = S.msgAdd state "Keep that filthy corpse out of my shop!" in
                sp.inventory, [], sp.gold
            | value when value <= 0 ->
                S.msgAdd state "You can just leave that here.";
                iRemain, iDropped, sp.gold
            | _ when sp.gold >= goldMax ->
                S.msgAdd state "Oh Croesus, thank you for your donation!";
                iRemain, iDropped, sp.gold
            | value ->
                S.msgAdd state (C.sf "Thank you! Here's %i gold for you." value);
                iRemain, iDropped, sp.gold + value
            )
        | false ->
            iRemain, iDropped, sp.gold
    in

    let player = { sp with inventory; gold } in

    let m' = Matrix.set { t with items = Items.concat dropped t.items } sp.pos m in
    { (SL.setMap m' state) with player }

let weightItems items =
    L.map Item.weight items
    |> L.fold_left (+) 0

let logItemsPickedUp items state =
    let getName i =
        let price = if SP.isInShop state then C.sf " (%i gold)" (Item.getPriceShop i) else "" in
        C.sf "%s%s" (Item.nameDisplay i) price
    in
    match items with
    | [] -> ()
    | _ when List.length items > Config.itemsDisplayedMax ->
        S.msgAdd state "You pick up many items."
    | item::[] ->
        C.sf "You pick up %s." (getName item) |> S.msgAdd state

    | _ ->
        let _ = List.iter (fun i -> getName i |> S.msgAdd state) items in
        S.msgAdd state "You pick up these items:"

let pickup sl (state : S.t) =
    let sp = state.player in
    let m = SL.map state in
    let t = Matrix.get m sp.pos in

    let iTaken, iRemain = Items.takeSelection t.items sl in

    let weightWielded = Option.fold ~none:0 ~some:(fun w -> Item.weight w) sp.weaponWielded in
    let weightTotal = weightItems iTaken + weightItems sp.inventory + weightWielded in
    if weightTotal > sp.inventoryWeightMax then
        let _ = S.msgAdd state "That would be more than you can carry." in
        state
    else

    let goldTaken, iTaken =
        L.partition_map
            (fun (item : Item.t) -> match item.t with
            | Gold -> Left (Item.count item)
            | _ -> Right item
            )
            iTaken
    in
    let totalGoldTaken = List.fold_left (+) 0 goldTaken in

    if SP.isInShop state then
        match totalGoldTaken, iTaken with
        | goldTaken, _ when goldTaken > 0 -> S.msgAdd state "Hey! That's not your gold!"; state
        | _, iTaken ->
            let itemsValue = L.fold_left (fun t i -> t + (Item.getPriceShop i)) 0 iTaken in
            if itemsValue > sp.gold then
                let _ = S.msgAdd state "You can't afford that!" in
                state
            else
                let _ = logItemsPickedUp iTaken state in
                let gold = sp.gold - itemsValue in
                let player = { sp with inventory = Items.concat iTaken sp.inventory; gold } in
                let m' = Matrix.set { t with items = iRemain } sp.pos m in
                { (SL.setMap m' state) with player }

    else

    let _ = if totalGoldTaken > 0 then S.msgAdd state (C.sf "You pick up %i gold." totalGoldTaken) in
    let _ = logItemsPickedUp iTaken state in
    let gold = sp.gold + totalGoldTaken in
    let player = { sp with inventory = Items.concat iTaken sp.inventory ; gold } in
    let m' = Matrix.set { t with items = iRemain } sp.pos m in
    { (SL.setMap m' state) with player }

let throw (si : C.selectionItem) dir (state : S.t) =
    let rangeThrown = 6 in (* TODO *)
    let sp = state.player in

    let item, inventory = Items.splitIndex sp.inventory si.iIndex (C.Count 1) in

    let msgThrower = C.sf "You throw the %s." (Item.name item) in (* TODO a vs an *)
    let state = Attack.throw item sp.pos dir rangeThrown msgThrower state in
    let sp = state.player in

    let player = { sp with inventory } in

    { state with player }

let moveToStairs ~dir (state : S.t) =
    let stairType = match dir with
        | Map.Up -> Map.StairsUp
        | Down -> StairsDown
    in
    let m = SL.map state in
    let posStairs = Map.getPosTerrain m stairType in
    let t = Matrix.get m posStairs in
    let m' = Matrix.set { t with occupant = Some Player } posStairs m in
    let player = { state.player with pos = posStairs } in
    { (SL.setMap m' state) with player;  }

let maybeAddHp (state : S.t) =
    let interval = 15 / state.player.level |> max 3 in
    match state with
    | state when state.mode <> Playing -> state
    | state -> UpdatePlayer.addHp (if R.oneIn interval then 1 else 0) state

let rec handleParalysis (state : S.t) = match state with
    | _ when state.mode <> Playing -> Some state
    | _ ->
        let sp = state.player in
        let paralysis = L.map (function | C.Paralyzed i -> i) sp.status in
        let wasParalyzed = L.is_empty paralysis |> not in

        let status, stillParalyzed = match paralysis with
            | [] -> [], false
            | i::_ when i <= 0 -> C.listRemove (C.Paralyzed i) sp.status, false
            | i::_ ->
                C.listRemove (C.Paralyzed i) sp.status
                |> L.cons (C.Paralyzed (i - 1))
                , true
        in

        let state = UpdatePlayer.setStatus status state in

        if stillParalyzed then
            let _ = View.imageCreate  state in
            afterAction state
        else
            let _ = if wasParalyzed then S.msgAdd state "You can move again." in
            Some state

and afterAction state =
    state
    |> UpdateMap.rotCorpses
    |> UpdatePlayer.knowledgeMap
    |> Ai.animateCreatures
    (* TODO update playerMap after each creature move *)
    |> Ai.maybeSpawnCreatures
    |> Ai.maybeHarassPlayer
    |> S.incTurns
    |> UpdatePlayer.knowledgeCreaturesDelete
    |> UpdatePlayer.knowledgeMap
    |> checkHp
    |> maybeAddHp
    |> handleParalysis

let rec logTidy (state : S.t) =
    let log = state.messages in
    if Queue.length log <= Config.logSizeMax then () else

    let _ = Queue.take log in
    logTidy state

let action a (state : S.t) =
    logTidy state;
    ( match a with
    | Absorb -> absorb state
    | BlindUnblind -> blindUnblind state
    | Close dir -> close dir state
    | Drop sl -> drop sl state
    | MoveDir mf -> move mf state
    | Pickup sl -> pickup sl state
    | Quaff si -> quaff si state
    | Read si -> read si state
    | Search -> search state
    | Throw (si, dir) -> throw si dir state
    | Wield si -> wield si state
    | Zap (si, dir) -> zap si dir state
    )
    |> afterAction
