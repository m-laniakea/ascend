open Matrix

module C = Common
module P = Position
module R = Random_
module S = State
module SL = StateLevels

let reduceDamage ac damage =
    if ac >= 0 then damage else
    (R.rn ac (-1)) + damage |> min 1

let getHitThreshold ac attackerLevel =
    let hitThresholdBase = 10 in
    let ac' = if ac < 0 then R.rn ac (-1) else ac in
    hitThresholdBase + ac' + attackerLevel |> max 1

let throw item pFrom dir range msgThrower state =
    let m = SL.map state in
    let isMiss () = R.oneIn 4 in
    let sourceIsPlayer = pFrom = state.player.pos in
    let dirOpposite = P.dirOpposite dir in

    let rec getPosStart pc posLanded = match P.step pc dir with
        | _ when pc = posLanded -> None
        | _ when Sight.playerCanSee state pc -> Some (P.step pc dirOpposite)
        | pn -> getPosStart pn posLanded
    in
    let rec getPosEnd pc posLanded = match P.step pc dir with
        | _ when posLanded = pc -> pc
        | pn when not (Sight.playerCanSee state pn) -> pc
        | pn -> getPosEnd pn posLanded
    in
    let msgAddCreatureMissed (c : Creature.t) p = if Sight.playerCanSee state p then
            S.msgAdd state (C.sf "The %s misses the %s." (Item.name item) c.info.name)
    in
    let msgAddCreatureHit (c : Creature.t) p = if Sight.playerCanSee state p then
        S.msgAdd state (C.sf "The %s hits the %s." (Item.name item) c.info.name)
    in

    let rollDamage () = match item with
        | Item.Weapon w -> R.roll w.damage
        | _ -> R.rn 1 2
    in

    ( if Sight.playerCanSee state pFrom
        then
            S.msgAdd state msgThrower
        else
            S.msgAdd state (C.sf "You see a %s fly." (Item.name item))
            (* Todo blindness *)
    );

    let rec doThrow item dir pc range state = match P.step pc dir with
        | _ when range <= 0 -> state, pc
        | pn when not (Map.isInMap pn) -> state, pc
        | pn ->
            ( match Matrix.get m pn with
            | { occupant = Some Boulder; _ } -> doThrow item dir pn (range - 1) state
            | { occupant = Some (Creature c); _ } as t ->
                ( match isMiss () with
                | true ->
                    msgAddCreatureMissed c pn;
                    doThrow item dir pn (range - 1) state
                | false ->
                    msgAddCreatureHit c pn;
                    let damage = rollDamage () in
                    UpdateCreature.addHp ~sourceIsPlayer (-damage) t pn c state, pn
                )
            | { occupant = Some Player; _ } ->
                ( match isMiss () with
                | true ->
                    S.msgAdd state (C.sf "The %s misses you." (Item.name item));
                    doThrow item dir pn (range - 1) state
                | false ->
                    S.msgAdd state (C.sf "The %s hits you." (Item.name item));
                    let damage = rollDamage () in
                    (UpdatePlayer.addHp (-damage) state), pn
                )
            | t ->
                if Map.isTileTypeWalkable t then
                    doThrow item dir pn (range - 1) state
                else
                    state, pc
            )
    in

    let state, posLanded = doThrow item dir pFrom range state in
    let posStart = getPosStart pFrom posLanded in

    ( match posStart with
        | None -> () (* At no point did player see the item *)
        | Some ps ->

            let animation =
                View.
                { dir
                ; posStart = ps
                ; posCurrent = ps
                ; posEnd = getPosEnd ps posLanded
                ; image = imageOfItem item
                }
            in
            View.animate state ~cumulative:false ~linger:false animation;
    );

    let m = SL.map state in
    let tLanded = Matrix.get m posLanded in
    let tLanded = { tLanded with items = item::tLanded.items } in
    let m' = Matrix.set tLanded posLanded m in

    SL.setMap m' state

let getReflectedDir p (dir : P.dir) state =
    assert (Map.isInMap p);
    let dirReversed = P.dirOpposite dir in
    if 0 = dir.dr || 0 = dir.dc then dirReversed else
    (* ^ easy case for + shaped rays *)

    let m = SL.map state in

    let tAlongDirCol = P.step p { dirReversed with dr = 0 } |> Matrix.get m in
    let tAlongDirRow = P.step p { dirReversed with dc = 0 } |> Matrix.get m in

    let dirReflectRow = P.dirRevRow dir in
    let dirReflectCol = P.dirRevCol dir in

    match Map.isTileTypeWalkable tAlongDirCol, Map.isTileTypeWalkable tAlongDirRow with
    | false, false -> dirReversed (* room (internal) corner *)
    | true, false -> dirReflectCol (* vertical reflection *)
    | false, true -> dirReflectRow (* horizontal reflection *)
    | true, true -> R.item [ dirReflectCol; dirReflectRow ] (* exposed corner *)

let castRay (effect : Hit.effect) from dir roll (state : S.t) =
    let reductionRangeOnHit = 3 in
    let damage =
        R.roll roll
    in
    let range = 20 + R.rn 1 8 in
    let sourceIsPlayer = from = state.player.pos in

    let msgs = Hit.getMsgsEffect effect in
    let rec doRay from pc dir state range =
        if range <= 0 then state else
        let pn = P.step pc dir in
        if not (Map.isInMap pn) then state else
        (* TODO if hitting edge of map...
            should still reflect instead of fizzing out
        *)

        let state, shouldReflect, range =
            let m = SL.map state in match Matrix.get m pn with
            | { occupant = Some Boulder; _ } as t ->
                ( match effect with
                | Sonic ->
                    let rocks = Item.rock (6 + R.rn 1 6) in
                    let t = { t with occupant = None; items = rocks::t.items } in
                    let m = Matrix.set t pn m in
                    ( if Sight.playerCanSee state pn then
                        S.msgAdd state (C.sf "The %s crumbles the boulder." msgs.msgCause)
                    else
                        S.msgAdd state "You hear rock crumbling."
                    );
                    SL.setMap m state, false, range - reductionRangeOnHit
                | _ ->
                    S.msgAdd state (C.sf "The %s whizzes past the boulder." msgs.msgCause);
                    state, false, range
                )
            | { occupant = Some Creature c; _ } as t ->
                ( if Sight.playerCanSee state pn then
                    S.msgAdd state (C.sf "The %s %s the %s." msgs.msgCause msgs.msgEffect c.info.name)
                );
                UpdateCreature.addHp ~sourceIsPlayer (-damage) t pn c state, false, range - reductionRangeOnHit
                (* ^TODO resistances *)
            | { occupant = Some Player; _ } ->
                S.msgAdd state (C.sf "The %s %s you!" msgs.msgCause msgs.msgEffect);
                UpdatePlayer.addHp (-damage) state, false, range - reductionRangeOnHit
                (* ^TODO resistances *)

            | { occupant = None; _ } as t ->
                if not (Map.isTileTypeWalkable t) then
                    state, true, range
                else
                    state, false, range (* TODO burning items, etc. *)
        in

        if shouldReflect || range <= 1 then
            let _ = match Hit.getImageForAnimation effect dir with
            | None -> ()
            | Some image ->
                let animation =
                    View.
                    { dir
                    ; posStart = from
                    ; posCurrent = from
                    ; posEnd = if shouldReflect then pc else pn
                    ; image
                    }
                in
                View.animate ~linger:(range <= 1) state animation
            in

            if shouldReflect then
                doRay pn pn (getReflectedDir pn dir state) state (range - 1)
            else
                state
        else
            doRay from pn dir state (range - 1)
    in
    doRay from from dir state range

