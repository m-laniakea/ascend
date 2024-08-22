open Matrix

module M = Matrix
module P = Position
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

    let sp = state.player in
    let inventory = Item.rotCorpses state.turns sp.inventory in

    let player = { sp with inventory } in
    let state = { state with player } in
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

let digTile (t : Map.tile) = match t with
    | t when Map.isTileTypeWalkable t -> t
    | t ->
        ( match t.t with
        | Door (_, ori) -> { t with t = Door (Broken, ori) }
        | Wall _ -> { t with t = Floor }

        | Unseen -> assert false

        | Floor
        | Hallway HallRegular
        | StairsDown | StairsUp
            -> t

        | Hallway HallHidden
        | Stone
            -> { t with t = Hallway HallRegular }
        )

let dig from dir range state =
    let rec aux p range state =
        if range <= 0 || not (Map.isInMap p) then state else

        let m = SL.map state in
        let t = digTile (M.get m p) in
        let m = M.set t p m in
        let state = SL.setMap m state in

        let p = P.step p dir in

        aux p (range - 1) state

    in
    aux from range state
