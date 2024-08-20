open State

module L = List

let level state =
    let sl = state.stateLevels in
    List.nth sl.levels sl.indexLevel

let map state =
    (level state).map

let depth state =
    let sl = state.stateLevels in
    sl.indexLevel + 1

let depthNext state = (depth state) + 1

let setMap m state =
    let sl = state.stateLevels in
    let cl = level state in
    let nl = C.listSet sl.indexLevel { cl with map = m } sl.levels in
    let sln = {sl with levels = nl} in
    { state with stateLevels = sln }

let setIndexLevel i state =
    let sl = state.stateLevels in

    assert (i >= 0);
    assert (i < List.length sl.levels);

    let sl' = { sl with indexLevel = i } in
    { state with stateLevels = sl' }


let levelAdd m state =
    let sl = state.stateLevels in
    let sln =
        { levels = sl.levels @ [m]
        ; indexLevel = sl.indexLevel + 1
        }
    in
    { state with stateLevels = sln }

let isLit p state =
    (level state).rooms
    |> L.exists
        ( function
        | { Map.lit = false; _ } -> false
        | room -> Map.isInRoom (Map.getOutliningRoom room) p
        )
