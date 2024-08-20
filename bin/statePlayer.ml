module L = List

module S = State
module SL = StateLevels

let ac (state : S.t) = 10 - state.statePlayer.acBonus

let room (state : S.t) =
    let sp = state.statePlayer in
    let roomsWithPlayer =
        (SL.level state).rooms
        |> L.filter ( fun room -> Map.isInRoom room sp.pos )
    in
    assert (L.length roomsWithPlayer <= 1);
    L.nth_opt roomsWithPlayer 0

