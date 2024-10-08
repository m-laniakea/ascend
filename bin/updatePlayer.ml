open Matrix

module L = List

module C = Common
module R = Random_
module S = State
module SL = StateLevels
module SP = StatePlayer

let regenBreak (state : S.t) =
    let sp = state.player in
    let regen = sp.regen in
    (if regen.active then S.msgAdd state "Your regeration is rudely interrupted.");
    let regen = S.{ active = false; points = 0 } in
    let player = { sp with regen } in
    { state with player }

let addHp n (state : S.t) =
    let state = if n < 0 then regenBreak state else state in
    let sp = state.player in
    let hp = sp.hp in
    let hp' = min (hp + n) sp.hpMax |> max 0 in
    let player = { sp with hp = hp' } in
    { state with player }

let knowledgeMapAddEmpty (state : S.t) =
    let level_t = SL.levelType state in
    let knowledgeEmpty = S.{ rooms = []; map = Matrix.fill Map.size Map.unseenEmpty; level_t } in

    let knowledgeLevels = state.player.knowledgeLevels @ [knowledgeEmpty] in
    let player = { state.player with knowledgeLevels } in
    { state with player }

let knowledgeMapTileType tt p (state : S.t) =
    let pk = S.getKnowledgeCurrentMap state in
    let t = Matrix.get pk p in
    let t = { t with t = tt } in
    let pk = Matrix.set t p pk in
    S.setKnowledgeCurrentMap pk state

let knowledgeMapTileOccupant o p (state : S.t) =
    let pk = S.getKnowledgeCurrentMap state in
    let t = Matrix.get pk p in
    let t = { t with occupant = o } in
    let pk = Matrix.set t p pk in
    S.setKnowledgeCurrentMap pk state

let revealRoomTilesIfLit (state : S.t) =
    if Creature.isBlind state.player.attributes then state else
    let updateKnowledge room state =
        let m  = SL.map state in

        L.fold_left
        ( fun state p ->
            let actual = Matrix.get m p in
            knowledgeMapTileType actual.t p state
        )
        state (Map.getOutliningRoom room |> Map.getRoomPositions)
    in

    match StatePlayer.room state with
    | None -> state
    | Some room -> match room with
        | { lit = false; _ } -> state
        | room -> updateKnowledge room state

let knowledgeMap state =
    let m  = SL.map state in
    let pk = S.getKnowledgeCurrentMap state in
    let pkUpdated = Matrix.foldI
        ( fun _ p pk' known ->
            let actual = Matrix.get m p in
            if actual <> known && Sight.playerCanSee state p then
                Matrix.set actual p pk'
            else
                pk'
        ) pk pk
    in
    S.setKnowledgeCurrentMap pkUpdated state
    |> revealRoomTilesIfLit

let knowledgeCreaturesDelete state =
    let pk = S.getKnowledgeCurrentMap state in
    let pk' = Matrix.map
        ( fun (v : Map.tile) -> match v with
            | { occupant = Some (Creature _); _ } ->
                { v with occupant = None }
            | { occupant = Some Player; _ } ->
                { v with occupant = None }
            | _ -> v
        ) pk
    in
    S.setKnowledgeCurrentMap pk' state

let addHpMax (sp : S.player) =
    let hpBonus = (R.rn 1 8 + 1 |> max 3) in
    let hpMax = sp.hpMax + hpBonus in
    let hp = sp.hp + hpBonus in

    { sp with hp; hpMax }

let addAc (sp : S.player) =
    { sp with acBonus = sp.acBonus + 1 }

let levelTo levelNew (sp : S.player) =
    let lDiff = levelNew - sp.level in
    assert (lDiff > 0);
    (* TODO level down *)
    let sp =
        C.repeat lDiff addHpMax sp
        |> C.repeat lDiff addAc
    in
    { sp with level = levelNew }

let xpAdd n (state : S.t) =
    let sp = state.player in
    let xp = sp.xp + n |> max 0 in
    let levelNew = SP.level xp in

    let sp = if levelNew <> sp.level then
        let _ = S.msgAdd state (C.sf "Welcome to level %i!" levelNew) in
        levelTo levelNew sp
    else
        sp
    in
    let player = { sp with xp } in

    { state with player }

let setStatus status (state : S.t) =
    let sp = state.player in
    let player = { sp with status } in
    { state with player }

let paralyze n (state : S.t) =
    let sp = state.player in
    let status = sp.status in

    let paralysis = L.map (function | C.Paralyzed i -> i) status in
    let status = match paralysis with
        | [] -> (C.Paralyzed n)::status
        | i::_ -> C.listRemove (C.Paralyzed i) status |> L.cons (C.Paralyzed (max i n))
    in

    setStatus status state

let addAttribute a (state : S.t) =
    let player = state.player in
    let attributes = player.attributes in

    if C.contains attributes a then state else
    let attributes = a::attributes in

    let player = { player with attributes } in
    { state with player }
