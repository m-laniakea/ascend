open Matrix

module L = List

module C = Common
module Cr = Creature
module M = Matrix
module P = Position
module R = Random_
module S = State
module SL = StateLevels

let rangeThrownCreature = 8

let canMoveHere c m p =
    (* ^TODO naming *)
    match (M.get m p : Map.tile) with
        (* TODO water, phasing, etc. *)
        | { occupant = Some _; _ } -> false
        | { occupant = None; t = t; _ } ->
            ( match t with
                | Door (Broken, _) -> true
                | Door (Open, _) -> true
                | Door (Closed, _) -> not (Cr.hasAttribute c Cr.NoHands)
                | Door (Hidden, _) -> false
                | Floor -> true
                | Hallway HallHidden -> false
                | Hallway HallRegular -> true
                | StairsDown -> true
                | StairsUp  -> false
                | Stone -> false
                | Unseen -> false
                | Wall Horizontal | Wall Vertical -> false
            )

let canSpawnAt ?(forbidPos=None) (m : Map.t) p =
    if Some p = forbidPos then
        false
    else
    match Matrix.get m p with
        (* TODO water, phasing, etc. *)
        | { occupant = Some _; _ } -> false
        | { occupant = None; t = t; _ } ->
            ( match t with
                | Door (Broken, _) -> true
                | Door (Open, _) -> true
                | Door (Closed, _) | Door (Hidden, _) -> false
                | Floor -> true
                | Hallway HallHidden -> false
                | Hallway HallRegular -> true
                | StairsDown -> true
                | StairsUp  -> false
                | Stone -> false
                | Unseen -> false
                | Wall Horizontal | Wall Vertical -> false
            )

let isItemMatchingHere f m p =
    Matrix.get m p
    |> (fun (t : Map.tile) -> L.exists f t.items)

let findPosItemMatchingInSight c f from state =
    SL.map state
    |> Matrix.mapIFindAll
        ( fun m p (t : Map.tile) -> match L.exists f t.items with
            | false -> None
            | true when from <> p && not (canMoveHere c m p) -> None
            | true -> Some p
        )
    |> L.filter (fun p -> Sight.creatureCanSee c from p state)
    |> L.sort (P.closestTo from)
    |> C.hdOpt

let getSpawnPosition ?(preferNear=None) ~room state =
    let m = SL.map state in
    let pp = state.player.pos in
    let spawnPositions =
        Map.allMapPositions
        |> L.filter (fun p -> canSpawnAt ~forbidPos:(Some pp) m p)
    in
    let pInView, pOutOfView =
        spawnPositions
        |> L.partition (fun p -> Sight.playerCanSee state p)
    in

    let pInView, pOutOfView = match room with
        | None -> pInView, pOutOfView
        | Some r ->
              L.filter (Map.isInRoom r) pInView
            , L.filter (Map.isInRoom r) pOutOfView
    in
    match preferNear with
        | None ->
            ( match pOutOfView, pInView with
            | [], [] -> None
            | (_::_ as oov), _ -> Some (R.item oov)
            | _, pOk -> Some (R.item pOk)
            )
        | Some p ->
            let sortClosest = L.sort (P.closestTo p) in
            let closestInView = sortClosest pInView in
            let closestOutOfView = sortClosest pOutOfView in

            let closest = if p = state.player.pos then
                    closestInView @ closestOutOfView
                else
                    closestOutOfView @ closestInView
            in
            L.nth_opt closest 0

let placeCreatures creatures ?(preferNear=None) ~room state =
    let rec aux creatures ~preferNear state = match creatures with
        | [] -> state
        | creature::tl ->
            ( match getSpawnPosition ~preferNear ~room state with
            | None -> state
            | Some p ->
                let map = SL.map state in
                let t = Matrix.get map p in
                let t' = { t with occupant = Some (Creature creature) } in
                let map' = Matrix.set t' p map in
                let state = SL.setMap map' state in
                let preferNear = Some (Option.fold ~none:p ~some:C.id preferNear) in

                aux tl ~preferNear state
            )
    in
    aux creatures ~preferNear state

let spawnCreatures ?(preferNear=None) ~room state =
    let d = SL.depth state in (* TODO difficulty ob1 on level gen *)
    let creatures = Cr.random d in

    placeCreatures creatures ~preferNear ~room state

let getCreatureMoveNext pGoal c m p =
    Map.posAround p
    |> List.filter (fun p -> pGoal = p || canMoveHere c m p)

let moveCreature a b (state : S.t) =
    let m = SL.map state in
    let ct = Matrix.get m a in
    let tt = Matrix.get m b in
    match tt with
    | { occupant = Some _; _ } ->
        (* TODO displacing creature *)
        a, state
    | { t = Door (Closed, ori); _ } ->
        let m = Matrix.set { ct with occupant = None } a m in
        let m = Matrix.set { tt with t = Door (Open, ori); occupant = ct.occupant } b m in
        S.msgAdd state "You hear a door open.";
        b, SL.setMap m state
    | _ ->
        let m = Matrix.set { ct with occupant = None } a m in
        let m = Matrix.set { tt with occupant = ct.occupant } b m in
        b, SL.setMap m state

let getCreaturePath c m start goal =
  let open AStar in
  let open AStar in
    let problem =
        { cost = P.distanceManhattan
        ; goal
        ; get_next_states = getCreatureMoveNext goal c m
        }
    in
    match search problem start with
    | None -> None
    | Some path when List.length path <= 1 -> Some []
    | Some path ->
        Some
        ( path
        |> List.rev
        |> List.tl
        )

let creatureAttackMelee (c : Creature.t) p (state : S.t) =
    let isTargetPlayer = p = state.player.pos in
    let m = SL.map state in
    let targetExists m = isTargetPlayer || Map.getCreatureAtOpt m p |> Option.is_none |> not in

    assert (targetExists m);

    let nameTarget =
        if isTargetPlayer then
            "you"
        else
            let c = Map.getCreatureAt m p in
            C.sf "the %s" c.info.name
    in
    let ac = if isTargetPlayer then StatePlayer.ac state else c.info.acBase in
    let hitThreshold = Attack.getHitThreshold ac c.level in

    let rec aux hits state =
        let m = SL.map state in
        if not (targetExists m) then state else
        match hits with
        | [] -> state
        | (addSides, h)::htl -> match R.rollAttackLanded hitThreshold addSides with
            | Miss ->
                let _ = if Sight.playerCanSee state p then
                    S.msgAdd state (C.sf "The %s misses %s." c.info.name nameTarget)
                in
                state
            | MissBarely ->
                let _ = if Sight.playerCanSee state p then
                    S.msgAdd state (C.sf "The %s just misses %s." c.info.name nameTarget)
                in
                state
            | Hit ->
                let damage =
                    ( match h with
                    | Hit.Melee hm -> R.roll hm.stats.roll
                    | Hit.Weapon h ->
                        ( match Item.getWeaponMostDamaging c.inventory with
                        | None -> R.roll h
                        | Some w -> (R.roll h) + (R.roll w.damage)
                        )
                    | _ -> assert false
                    )
                    |> Attack.reduceDamage ac
                in

                let msgsHit = Hit.getMsgs h in
                let _ = if Sight.playerCanSee state p then
                        S.msgAdd state (C.sf "The %s %s %s." c.info.name msgsHit.msgHit nameTarget)
                    else if Cr.isPet c then
                        S.msgAdd state "You hear some noises."
                in

                match isTargetPlayer with
                | true ->
                    let state = UpdatePlayer.addHp (-damage) state in
                    aux htl state
                | false ->
                    (* TODO creature passives *)
                    let c = Map.getCreatureAt m p in
                    let tile = Matrix.get m p in
                    let state = UpdateCreature.addHp ~sourceIsPlayer:false (-damage) tile p c state in
                    aux htl state
    in
    let hits =
        c.info.hits
        |> List.filter (function | Hit.Melee _ -> true | Hit.Weapon _ -> true | _ -> false)
        |> List.mapi (fun i v -> i, v)
    in
    aux hits state


let creatureThrow (c : Creature.t) p item dir state =
    let msgThrower = C.sf "The %s throws a %s." c.info.name (Item.name item) in (* TODO a vs an *)
    let state = Attack.throw item p dir rangeThrownCreature msgThrower state in

    let m = SL.map state in
    let tCreature = Matrix.get m p in
    let creature = Map.Creature { c with inventory = C.listRemove item c.inventory } in
    let tCreature = { tCreature with occupant = Some creature } in

    let m' = Matrix.set tCreature p m in
    SL.setMap m' state

let creatureAttackRanged (c : Creature.t) cp tp state =
    c.info.hits
    |> List.filter (function | Hit.Ranged _ -> true | Weapon _ -> true | _ -> false)
    |> List.fold_left
        ( fun state' h ->
            let pDiff = P.diff cp tp in
            let pDir = P.dir pDiff in

            match h with
                | Hit.Weapon _ ->
                    ( match Creature.getWeaponForThrow c with
                    | None -> assert false
                    | Some w -> creatureThrow c cp (Item.Weapon w) pDir state'
                    )
                | Ranged hr as h ->
                    let hs = hr.stats in
                    let msgsHit = Hit.getMsgs h in
                    S.msgAdd state (C.sf "The %s %s %s." c.info.name msgsHit.msgHit msgsHit.msgCause);
                    Attack.castRay (Hit.getEffect h) cp pDir hs.roll state'
                | _ -> assert false
        )
        state

let eatFloorItems (c : Creature.t) p state =
    let m = SL.map state in
    let t = Matrix.get m p in
    let comestibles, _ = L.partition Item.isComestible t.items in

    match comestibles with
    | [] -> assert false
    | cm::_ ->
        S.msgAdd state (C.sf "The %s munches on a %s." c.info.name (Item.name cm));
        let remain = C.listRemove cm t.items in
        let c = { c with hp = c.hpMax } in
        let t = { t with items = remain; occupant = Some (Creature c) } in
        let m' = Matrix.set t p m in
        SL.setMap m' state

let eatIfPetOrPickupItems (c : Creature.t) p state =
    let m = SL.map state in
    let t = Matrix.get m p in
    let weps, remain = L.partition Item.isWeapon t.items in
    let comestibles, _ = L.partition Item.isComestible t.items in

    match Cr.isPet c with
    | true when not (L.is_empty comestibles) -> eatFloorItems c p state
    | _ when Cr.hasAttackWeapon c.info ->
        let c = { c with inventory = weps @ c.inventory } in
        let t = { t with items = remain; occupant = Some (Creature c) } in
        let m' = Matrix.set t p m in
        SL.setMap m' state
    | _ -> state

let canAttackMelee pc pt = P.distance2 pc pt <= 2

let canAttackRanged c cp pt ~canSeeTarget =
    canSeeTarget
    && P.areLinedUp cp pt
    && Cr.hasAttackRanged c
    (* ^TODO check that attack has path to target *)

let canAttack c pc pt ~canSeeTarget =
    canAttackMelee pc pt
    || canAttackRanged c pc pt ~canSeeTarget

type path = P.t list

type target =
    | TargetApproach of path
    | TargetAttack of P.t
    | TargetPickup of path
    | TargetGoTo of path

let handleTarget c pc target state =
    match target with
    | TargetAttack pt when canAttackMelee pc pt ->
        pc, creatureAttackMelee c pt state
    | TargetAttack pt ->
        pc, creatureAttackRanged c pc pt state
    | TargetApproach path ->
        ( match path with
        | [] -> assert false
        | _::[] -> pc, state
        | pt::_ -> moveCreature pc pt state
        )
    | TargetGoTo path ->
        ( match path with
        | [] -> pc, state
        | pt::_ -> moveCreature pc pt state
        )
    | TargetPickup path ->
        ( match path with
        | [] -> pc, eatIfPetOrPickupItems c pc state
        | pt::_ -> moveCreature pc pt state
        )

let getTargetPet c cp state =
    let m = SL.map state in
    let posPlayer = state.player.pos in
    let followPlayer () = getCreaturePath c m cp posPlayer |> Option.map (fun p -> TargetApproach p) in
    let pathToItem = match findPosItemMatchingInSight c Item.isComestible cp state with
        | Some p -> getCreaturePath c m cp p
        | None -> None
    in

    match pathToItem with
    | Some p -> Some (TargetPickup p)
    | None ->
        let m = SL.map state in
        let targets =
            Map.getPositionsCreatureHostileNonSessile m
            |> L.filter (fun p -> Sight.creatureCanSee c cp p state)
            |> L.sort (P.closestTo cp)
        in
        let targetAttack, pathToEnemy = match L.nth_opt targets 0 with
            | Some pt when canAttack c cp pt ~canSeeTarget:true -> Some (TargetAttack pt), getCreaturePath c m cp pt
            | Some pt -> None, getCreaturePath c m cp pt
            | None -> None, None
        in
        match targetAttack, pathToEnemy with
        | Some _ as tAttack, _ -> tAttack
        | _, Some p -> Some (TargetApproach p)
        | _, None ->
            let randomAround = Map.posAround cp |> L.filter (canMoveHere c m) in
            match randomAround with
            | [] -> followPlayer ()
            | _ ->
                let shouldWalkRandomly =
                    P.distance2 cp posPlayer <= 32
                    && R.oneIn 3
                in
                match shouldWalkRandomly with
                | true ->
                    ( match getCreaturePath c m cp (R.item randomAround) with
                    | None -> followPlayer ()
                    | Some p -> Some (TargetGoTo p)
                    )

                | false -> followPlayer ()


let getTargetWhenHostile c pc state =
    let m = SL.map state in

    let posPlayer = state.player.pos in
    let canSeePlayer = Sight.creatureCanSee c pc posPlayer state in

    if canAttack c pc posPlayer ~canSeeTarget:canSeePlayer then Some (TargetAttack posPlayer) else

    let isItemDesired = Item.isWeapon in

    let shouldPickupItemsHereAnyway =
        c.hp > c.hpMax / 2 (* Rush to target if dying *)
        && Cr.hasAttackWeapon c.info
        && R.oneIn 2
        && isItemMatchingHere isItemDesired m pc
    in

    let pathToPlayer = getCreaturePath c m pc posPlayer |> Option.map (fun p -> TargetApproach p) in
    let pathItemDesired () =
        ( match Cr.hasAttackWeapon c.info with
        | true -> findPosItemMatchingInSight c isItemDesired pc state
        | false -> None
        )
        |> Option.fold ~none:None ~some:(fun pi -> getCreaturePath c m pc pi)
        |> Option.map (fun p -> TargetPickup p)
    in

    let pathToPetItemPlayer () =
        let targets =
            Map.getPositionsCreaturePet m
            |> L.filter (fun pt -> Sight.creatureCanSee c pc pt state)
            |> L.sort (P.closestTo pc)
        in
        let targetAttack, pathToEnemy = match L.nth_opt targets 0 with
            (* ^TODO refactor with that in getTargetPet *)
            | Some pt when canAttack c pc pt ~canSeeTarget:true -> Some (TargetAttack pt), getCreaturePath c m pc pt
            | Some pt -> None, getCreaturePath c m pc pt
            | None -> None, None
        in
        match targetAttack, pathToEnemy with
        | Some _ as tAtt, _  -> tAtt
        | _, Some p -> Some (TargetApproach p)
        | _, None ->
            ( match pathItemDesired () with
            | None -> pathToPlayer
            | sp -> sp
            )
    in

    match pathToPlayer with
    | Some _ when canSeePlayer && shouldPickupItemsHereAnyway -> Some (TargetPickup [])
    | Some _ as sp when canSeePlayer -> sp
    | _ -> pathToPetItemPlayer ()

let getTarget (c : Creature.t) pc (state : S.t) = match c.hostility with
    | Tame -> getTargetPet c pc state
    | Hostile -> getTargetWhenHostile c pc state

let rec animateCreature c cp (state : S.t) =
    if not (Cr.hasTurn c) then state else

    match getTarget c cp state with
    | None -> state
    | Some target ->
        let cpn, state' = handleTarget c cp target state in
        let c = { c with pointsSpeed = c.pointsSpeed - C.pointsSpeedPerTurn } in
        animateCreature c cpn state'

let animateCreatures state = Matrix.foldI
    ( fun _ p state' -> function
        | Map.{ occupant = Some (Creature c); _ } ->
            let m = SL.map state' in
            ( match Map.getCreatureAtOpt m p with
            | None -> state'
            | Some cc -> if c != cc then state' else
                (* ^TODO guarantee not strong enough; creatures probably need UID *)
                animateCreature cc p state'
            )
        | _ -> state'
    ) state (SL.map state)

let maybeSpawnCreatures state =
    if R.oneIn 50 then
        spawnCreatures ~room:None state
    else
        state

let doCreaturePassive c state =
    let pAttacks = Cr.getAttacksPassive c in
    if L.is_empty pAttacks then state else

    L.fold_left
        ( fun state (pa : Hit.passive) ->
            let msgs = Hit.getMsgs (Hit.Passive pa) in
            S.msgAdd state (C.sf "It %s. The %s %s you." msgs.msgHit msgs.msgCause msgs.msgEffect);
            let effectSize = R.roll { rolls = c.info.levelBase + 1; sides = pa.maxRoll } in
            (* ^TODO based on current level *)

            match pa.effect with
            (* ^TODO centralize *)
            | Paralyze -> UpdatePlayer.paralyze effectSize state
            | _ ->
                UpdatePlayer.addHp (-effectSize) state
                (* ^TODO resistances *)
        )
        state
        pAttacks

let movePetsFromLevel (stateOld : S.t) (state : S.t) =
    let posOld = stateOld.player.pos in
    let mOld = SL.map stateOld in

    let m, pets =
        Map.posAround posOld
        |> L.fold_left
            ( fun (m, pets) p -> match Map.getCreatureAtOpt m p with
            | None -> m, pets
            | Some c when c.hostility = Tame ->
                let t = Matrix.get m p in
                let t = { t with occupant = None } in
                Matrix.set t p m, c::pets

            | _ -> m, pets
            )
        (mOld, [])
    in
    let state' = SL.setIndexLevel stateOld.levels.indexLevel state in
    let state' = SL.setMap m state' in
    SL.setIndexLevel state.levels.indexLevel state'
    |> placeCreatures pets ~preferNear:(Some state.player.pos) ~room:None
