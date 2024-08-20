open Matrix

module L = List

module R = Random_
module S = State
module SL = StateLevels

let addHp n (state : S.t) =
    let sp = state.statePlayer in
    let hp = sp.hp in
    let hp' = min (hp + n) sp.hpMax |> max 0 in
    let statePlayer = { sp with hp = hp' } in
    { state with statePlayer }

let knowledgeMapAddEmpty (state : S.t) =
    let knowledgeEmpty = S.{ rooms = []; map = Matrix.fill Map.size Map.unseenEmpty } in

    let knowledgeLevels = state.statePlayer.knowledgeLevels @ [knowledgeEmpty] in
    let statePlayer = { state.statePlayer with knowledgeLevels } in
    { state with statePlayer }

let revealRoomIfLit state =
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

