open Matrix

module M = Matrix
module SL = StateLevels

let rotCorpses state =
    let m = SL.map state in

    let m' = M.foldI
        ( fun _ p m (t : Map.tile) -> match t.items with
            | [] -> m
            | items ->
                let items = Item.rotCorpses state.turns items in
                M.set { t with items } p m
        ) m m
    in

    let sp = state.statePlayer in
    let inventory = Item.rotCorpses state.turns sp.inventory in

    let statePlayer = { sp with inventory } in
    let state = { state with statePlayer } in
    SL.setMap m' state

let addItem ~gold state m p =
    let t = M.get m p in
    let i =
        if gold then
            let d = SL.depthNext state in
            Item.rnGold d
        else
            Item.random ()
    in

    let t' = Map.{ t with items = i::t.items } in
    M.set t' p m
