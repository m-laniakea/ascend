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

let setTileType tt p state =
    let m = SL.map state in
    let t = Matrix.get m p in
    let t = { t with t = tt } in
    let m = Matrix.set t p m in
    SL.setMap m state

let closeDoorsGnilsogIfAble state =
    match (SL.level state).level_t with
    | Final ->
        let m = SL.map state in

        let m =
            Levels.pDoorsGnilsog
            |> List.fold_left
                (fun m p ->
                    let tile = Matrix.get m p in
                    match tile with
                    | Map.{ occupant = Some _; _ } -> m
                    | { items = _::_; _ } -> m
                    | { t = Door (Open, dir); _ } as tile ->
                        let tile = { tile with t = Door (Closed, dir) } in
                        Matrix.set tile p m
                    | { t = Door _; _ } -> m
                    | _ -> failwith "not a (gnilsog) door"
                )
                m
        in
        SL.setMap m state


    | _ -> failwith "Asked to close gnilsog's doors on wrong level..."

let lowerDragonGate state =
    match (SL.level state).level_t with
    | Final -> setTileType Floor Levels.pDragonGate state
    | _ -> failwith "Asked to lower dragon gate on wrong level..."
