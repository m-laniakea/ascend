module L = List

module S = State
module SL = StateLevels

let ac (state : S.t) = 10 - state.player.acBonus

let room (state : S.t) =
    let sp = state.player in
    let roomsWithPlayer =
        (SL.level state).rooms
        |> L.filter ( fun room -> Map.isInRoom room sp.pos )
    in
    assert (L.length roomsWithPlayer <= 1);
    L.nth_opt roomsWithPlayer 0

let level xp =
    let levelMax = 17 in
    (* Level xp thresholds are 2^(lvl - 1) * 10 *)
    (* Add 3 just to make level 1 -> 2 slightly easier *)
    let rec aux i =
        if (1 lsl i) * 10 > xp + 3 then
            i + 1 |> min levelMax
        else
            aux (i + 1)
    in
    aux 0

let isInShop (state : S.t) =
    let sp = state.player in
    let pp = sp.pos in
    let cl = SL.level state in
    Map.isInShop cl.rooms pp
