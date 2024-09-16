open Matrix

module C = Common
module Cr = Creature
module P = Position
module R = Random_
module S = State
module SL = StateLevels

let reduceDamage ac damage =
    assert (damage >= 0);
    if damage = 0 then damage else
    if ac >= 0 then damage else
    (R.rn ac (-1)) + damage |> max 1

let getHitThreshold ac attackerLevel =
    let hitThresholdBase = 10 in
    let ac' = if ac < 0 then R.rn ac (-1) else ac in
    hitThresholdBase + ac' + attackerLevel |> max 1

let canThrownDomesticate item c = Item.isComestible item && Creature.isTameable c

let domesticateWithThrown item c pn state =
    let m = SL.map state in
    let ci = Creature.{ c.info with color = A.white } in
    let c =
        { c with Creature.hostility = Tame
        ; hp = c.hpMax
        ; info = ci
        }
    in
    let t = Matrix.get m pn in
    let t = { t with occupant = Some (Map.Creature c) } in
    let m = Matrix.set t pn m in

    S.msgAdd state (C.sf "The %s greedily devours the %s." c.info.name (Item.name item));
    (* ^TODO only if seen *)

    SL.setMap m state

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
            (* ^TODO blindness/not correct when you are not target *)
    );

    let rec doThrow item dir pc range state = match P.step pc dir with
        | _ when range <= 0 -> state, pc, true
        | pn when not (Map.isInMap pn) -> state, pc, true
        | pn ->
            ( match Matrix.get m pn with
            | { occupant = Some Boulder; _ } -> doThrow item dir pn (range - 1) state
            | { occupant = Some (Creature c); _ } ->
                ( match isMiss () with
                | true ->
                    msgAddCreatureMissed c pn;
                    doThrow item dir pn (range - 1) state
                | false ->
                    if canThrownDomesticate item c then
                        domesticateWithThrown item c pn state, pn, false
                    else
                        let _ = msgAddCreatureHit c pn in
                        let damage = rollDamage () in
                        UpdateCreature.addHp ~sourceIsPlayer (-damage) pn c state, pn, true
                )
            | { occupant = Some Player; _ } ->
                ( match isMiss () with
                | true ->
                    S.msgAdd state (C.sf "The %s misses you." (Item.name item));
                    doThrow item dir pn (range - 1) state
                | false ->
                    S.msgAdd state (C.sf "The %s hits you." (Item.name item));
                    let damage = rollDamage () in
                    (UpdatePlayer.addHp (-damage) state), pn, true
                )
            | t ->
                if Map.isTileTypeWalkable t then
                    doThrow item dir pn (range - 1) state
                else
                    state, pc, true
            )
    in

    let state, posLast, doesItemRemain = doThrow item dir pFrom range state in
    let posStart = getPosStart pFrom posLast in

    ( match posStart with
        | None -> () (* At no point did player see the item *)
        | Some ps ->

            let animation =
                View.
                { dir
                ; posStart = ps
                ; posCurrent = ps
                ; posEnd = getPosEnd ps posLast
                ; image = imageOfItem item
                }
            in
            View.animate state ~cumulative:false ~linger:false animation;
    );

    match doesItemRemain with
    | false -> state
    | true ->
        let m = SL.map state in
        let tLanded = Matrix.get m posLast in
        let tLanded = { tLanded with items = item::tLanded.items } in
        let m' = Matrix.set tLanded posLast m in

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

let getDamageGrave c effect =
    if not (Cr.isVulnerable c effect) then 0 else

    let hpMax = c.hpMax in
    match c.hp with
    | hp when hp <= hpMax / 4 -> hp
    | hp -> hp / 2 + 1

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
            let canSee = Sight.playerCanSee state pn in
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
            | { occupant = Some Creature c; _ } ->
                ( match Cr.isResistant c effect with
                | true ->
                    S.msgAddSeen state ~canSee (C.sf "The %s has no effect on the %s." msgs.msgCause c.info.name);
                    state, false, range - reductionRangeOnHit
                | false ->
                    let damage = match getDamageGrave c effect with
                        | dg when 0 = dg ->
                            S.msgAddSeen state ~canSee (C.sf "The %s %s the %s." msgs.msgCause msgs.msgEffect c.info.name);
                            damage
                        | dg ->
                            S.msgAddSeen state ~canSee (C.sf "The %s gravely wounds the %s." msgs.msgCause c.info.name);
                            max damage dg
                    in
                    UpdateCreature.addHp ~sourceIsPlayer (-damage) pn c state, false, range - reductionRangeOnHit
                )

            | { occupant = Some Player; _ } ->
                S.msgAdd state (C.sf "The %s %s you!" msgs.msgCause msgs.msgEffect);
                UpdatePlayer.addHp (-damage) state, false, range - reductionRangeOnHit
                (* ^TODO resistances *)

            | { occupant = None; t = Door (Closed, ori); _ } when effect = Fire ->
                (* TODO Other effects can harm doors as well *)
                ( match R.oneIn 3 with
                | true ->
                    S.msgAddSeen state ~canSee "The fire singes the door.";
                    state, false, 0
                | false ->
                    S.msgAddSeen state ~canSee "The fire reduces the door to ash.";
                    S.msgHearNotSeen state ~canSee "burning wood crackling violently.";
                    let state = UpdateMap.setTileType (Door (Broken, ori)) pn state in
                    state, false, range - reductionRangeOnHit * 2
                )

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

