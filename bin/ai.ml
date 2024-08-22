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

let findItemMatchingInSight c f from state =
    SL.map state
    |> Matrix.mapIFindAll
        ( fun m p (t : Map.tile) -> match L.exists f t.items with
            | false -> None
            | true when from <> p && not (canMoveHere c m p) -> None
            | true -> Some p
        )
    |> L.filter (fun p -> Sight.creatureCanSee c from p state)
    |> L.sort (P.closestTo from)

let placeCreature ?(preferNearby=false) ~room state =
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
    let closestFirst = L.sort (P.closestTo pp) in
    let creaturePos =
        if preferNearby then
            L.nth_opt
            ((closestFirst pInView) @ (closestFirst pOutOfView))
            0
        else
            match pOutOfView, pInView with
            | [], [] -> None
            | (_::_ as oov), _ -> Some (R.item oov)
            | _, pOk -> Some (R.item pOk)
    in
    match creaturePos with
        | None -> state
        | Some p ->
            let d = SL.depth state in (* TODO difficulty ob1 on level gen *)
            match Cr.random d with
                | None -> state
                | Some creature ->
                    let map = SL.map state in
                    let t = Matrix.get map p in
                    let t' = { t with occupant = Some (Creature creature) } in
                    let map' = Matrix.set t' p map in
                    SL.setMap map' state

let rec placeCreatures ?(preferNearby=false) ~room count state =
    if count <= 0 then state else
    let state = placeCreature ~preferNearby ~room state in
    placeCreatures ~preferNearby ~room (count - 1) state

let getCreatureMoveNext pGoal c m p =
    Map.posAround p
    |> List.filter (fun p -> pGoal = p || canMoveHere c m p)

let moveCreature a b state =
    let m = SL.map state in
    let ct = Matrix.get m a in
    let tt = Matrix.get m b in
    match tt with
    | { occupant = Some _; _ } ->
        (* TODO displacing creature *)
        state
    | { t = Door (Closed, ori); _ } ->
        let m = Matrix.set { ct with occupant = None } a m in
        let m = Matrix.set { tt with t = Door (Open, ori); occupant = ct.occupant } b m in
        S.msgAdd state "You hear a door open.";
        SL.setMap m state
    | _ ->
        let m = Matrix.set { ct with occupant = None } a m in
        let m = Matrix.set { tt with occupant = ct.occupant } b m in
        SL.setMap m state

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
    if p = state.player.pos then
        let hitThreshold = Attack.getHitThreshold StatePlayer.(ac state) c.level in
        c.info.hits
        |> List.filter (function | Hit.Melee _ -> true | Hit.Weapon _ -> true | _ -> false)
        |> List.mapi (fun i v -> i, v)
        |> List.fold_left
            ( fun state' (addSides, h) ->
                match R.rollAttackLanded hitThreshold addSides with
                | Miss ->
                    let _ = S.msgAdd state (C.sf "The %s misses you." c.info.name) in
                    state'
                | MissBarely ->
                    let _ = S.msgAdd state (C.sf "The %s just misses you." c.info.name) in
                    state'
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
                        |> Attack.reduceDamage StatePlayer.(ac state')
                    in

                    let msgsHit = Hit.getMsgs h in
                    S.msgAdd state (C.sf "The %s %s you." c.info.name msgsHit.msgHit);

                    UpdatePlayer.addHp (-damage) state'
            )
            state

    else
        let _ = assert false in
        (* TODO *)
        state

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

let creaturePickupWeapons (c : Creature.t) p state =
    let m = SL.map state in
    let t = Matrix.get m p in
    let weps, remain = L.partition Item.isWeapon t.items in

    let c = { c with inventory = weps @ c.inventory } in
    let t = { t with items = remain; occupant = Some (Creature c) } in
    let m' = Matrix.set t p m in
    SL.setMap m' state

let rec animateCreature c cp state =
    if not (Cr.hasTurn c) then state else
    let state = UpdatePlayer.knowledgeMap state in
    let pp = state.player.pos in
    (* ^TODO allow attacking other creatures *)
    let m = SL.map state in
    let canSeePlayer = Sight.creatureCanSee c cp pp state in

    let cpn, state' = if P.distance2 cp pp <= 2 then
        (* ^TODO blindness/confusion/etc. *)
            cp, creatureAttackMelee c pp state
        else if P.areLinedUp cp pp
                && Cr.hasAttackRanged c
                && canSeePlayer then
                (* ^TODO check that attack has path to target *)
            cp, creatureAttackRanged c cp pp state
        else
            let pathToPlayer = getCreaturePath c m cp pp in
            let pathToWeapon =
                if not (Cr.hasAttackWeapon c.info) then
                    None
                else
                    match findItemMatchingInSight c Item.isWeapon cp state with
                    | [] -> None
                    | wp::_ -> getCreaturePath c m cp wp
            in
            let moveAlongPath = function
                | None | Some [] -> cp, state
                | Some (np::_) -> np, moveCreature cp np state
            in
            match pathToWeapon with
                | None -> moveAlongPath pathToPlayer
                | Some [] when not canSeePlayer || R.oneIn 2 -> cp, creaturePickupWeapons c cp state
                | Some [] -> moveAlongPath pathToPlayer
                | Some _ when canSeePlayer -> moveAlongPath pathToPlayer
                | Some _ -> moveAlongPath pathToWeapon
    in
    let c = { c with pointsSpeed = c.pointsSpeed - C.pointsSpeedPerTurn } in
    animateCreature c cpn state'

let animateCreatures state = Matrix.foldI
    ( fun _ p state' -> function
        | Map.{ occupant = Some (Creature c); _ } ->
            let state = UpdatePlayer.knowledgeMap state' in
            animateCreature c p state
        | _ -> state'
    ) state (SL.map state)

let maybeAddCreature state =
    if R.oneIn 50 then
        placeCreature ~room:None state
    else
        state

let doCreaturePassive c state =
    let pAttacks = Cr.getAttacksPassive c in
    if L.is_empty pAttacks then state else

    L.fold_left
        ( fun state (pa : Hit.passive) ->
            let msgs = Hit.getMsgs (Hit.Passive pa) in
            S.msgAdd state (C.sf "It %s. The %s %s you." msgs.msgHit msgs.msgCause msgs.msgEffect);
            let damage = R.roll { rolls = c.info.levelBase + 1; sides = pa.maxRoll } in
            (* ^TODO based on current level *)
            UpdatePlayer.addHp (-damage) state
            (* ^TODO resistances *)
        )
        state
        pAttacks
