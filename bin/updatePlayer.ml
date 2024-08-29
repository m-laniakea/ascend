open Matrix

module L = List

module C = Common
module R = Random_
module S = State
module SL = StateLevels
module SP = StatePlayer

let addHp n (state : S.t) =
    let sp = state.player in
    let hp = sp.hp in
    let hp' = min (hp + n) sp.hpMax |> max 0 in
    let player = { sp with hp = hp' } in
    { state with player }

let knowledgeMapAddEmpty (state : S.t) =
    let knowledgeEmpty = S.{ rooms = []; map = Matrix.fill Map.size Map.unseenEmpty } in

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

let revealRoomIfLit (state : S.t) =
    if Creature.isBlind state.player.attributes then state else
    let updateKnowledge room state =
        let m  = SL.map state in
        let pk = S.getKnowledgeCurrentMap state in
        let pkUpdated = L.fold_left
            ( fun pk p ->
                let actual = Matrix.get m p in
                let known = Matrix.get pk p in
                if actual <> known then
                    Matrix.set actual p pk
                else
                    pk
            )
            pk (Map.getOutliningRoom room |> Map.getRoomPositions)
        in
        S.setKnowledgeCurrentMap pkUpdated state
    in

    match StatePlayer.room state with
    (* TODO blindness *)
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
    |> revealRoomIfLit

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

let rec addHpMax n (sp : S.player) =
    assert (n >= 0);
    if n = 0 then sp else

    let hpBonus = (R.rn 1 8 + 1 |> max 3) in
    let hpMax = sp.hpMax + hpBonus in
    let hp = sp.hp + hpBonus in

    addHpMax (n - 1) { sp with hp; hpMax }

let rec addAc n (sp : S.player) =
    assert (n >= 0);
    if n = 0 then sp else

    addAc (n - 1) { sp with acBonus = sp.acBonus + 1 }

let levelTo levelNew (sp : S.player) =
    let lDiff = levelNew - sp.level in
    assert (lDiff > 0);
    (* TODO level down *)
    let sp =
        addHpMax lDiff sp
        |> addAc lDiff
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
