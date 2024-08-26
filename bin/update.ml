open Matrix

module P = Position
module R = Random_
module S = State
module SL = StateLevels

let playerGoUp (state : S.t) =
    (* ^ TODO move to actionPlayer *)
    let p = state.player.pos in
    if Matrix.get (SL.map state) p |> Map.isTerrainOf StairsUp |> not then
        state
    else
        let sl = state.levels in
        if sl.indexLevel = 0 then
            state
        else
            SL.setIndexLevel (sl.indexLevel - 1) state
            |> UpdateMap.rotCorpses
            |> Player.moveToStairs ~dir:Down
            |> Ai.movePetsFromLevel state
            |> UpdatePlayer.knowledgeMap

let playerGoDown (state : S.t) =
    (* ^ TODO move to actionPlayer *)
    let p = state.player.pos in
    if Matrix.get (SL.map state) p |> Map.isTerrainOf StairsDown |> not then
        state
    else
        let sl = state.levels in
        if sl.indexLevel = List.length sl.levels - 1 then
            GenMap.gen state
            |> Player.moveToStairs ~dir:Up
            |> UpdatePlayer.knowledgeMapAddEmpty
            |> Ai.movePetsFromLevel state
            |> UpdatePlayer.knowledgeMap
        else
            SL.setIndexLevel (sl.indexLevel + 1) state
            |> UpdateMap.rotCorpses
            |> Player.moveToStairs ~dir:Up
            |> Ai.movePetsFromLevel state
            |> UpdatePlayer.knowledgeMap

let modeDead event state = match event with
    | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> None
    | _ -> Some state

let modePlaying event state =
    let open Map in
    match event with
    | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) -> Player.action (MoveDir west) state
    | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) -> Player.action (MoveDir east) state
    | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) -> Player.action (MoveDir north) state
    | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) -> Player.action (MoveDir south) state

    | `Key (`ASCII 'y', _) -> Player.action (MoveDir northWest) state
    | `Key (`ASCII 'u', _) -> Player.action (MoveDir northEast) state
    | `Key (`ASCII 'b', _) -> Player.action (MoveDir southWest) state
    | `Key (`ASCII 'n', _) -> Player.action (MoveDir southEast) state

    | `Key (`ASCII 'c', _) -> Select.dirClose state
    | `Key (`ASCII 's', _) -> Player.action Search state

    | `Key (`ASCII 'd', _) -> Select.drop state
    | `Key (`ASCII ',', _) -> Select.pickup state
    | `Key (`ASCII 'q', _) -> Select.quaff state
    | `Key (`ASCII 'r', _) -> Select.read state
    | `Key (`ASCII 't', _) -> Select.throw state
    | `Key (`ASCII 'w', _) -> Select.wield state
    | `Key (`ASCII 'z', _) -> Select.zap state

    | `Key (`ASCII '<', _) -> Some (playerGoUp state)
    | `Key (`ASCII '>', _) -> Some (playerGoDown state)
    | _ -> Some state

let modeSelecting event state s = match event with
    | `Key (`Escape, _) -> Some S.{ state with mode = Playing }
    | `Key (`ASCII k, _)  -> Select.handle k s state
    | _ -> Some state

let exec event (state : State.t) = match (state.mode : State.mode) with
    | Dead -> modeDead event state
    | Playing -> modePlaying event state
    | Selecting s -> modeSelecting event state s
