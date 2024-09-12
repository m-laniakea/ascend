open Matrix

module C = Common
module P = Position
module S = State

let center = Map.center

let levelGarden () =
    let mitras = Creature.mkMitras () in
    let info = { mitras.info with speed = 0 } in
    let mitras = { mitras with info } in
    let occupant = Some (Map.Creature mitras) in

    let colsQuarter = P.{ row = 0; col = Map.size.cols / 4 } in
    let rowsQuarter = P.{ row = Map.size.rows / 4; col = 0 } in
    let pavilionNW = center |> P.diff colsQuarter |> P.diff rowsQuarter in
    let pavilionNE = center |> P.add colsQuarter |> P.diff rowsQuarter in
    let pavilionSE = center |> P.add colsQuarter |> P.add rowsQuarter in
    let pavilionSW = center |> P.diff colsQuarter |> P.add rowsQuarter in

    let map =
        [ "-------------------------------------------------------------------------------"
        ; "|.............................................................................|"
        ; "|.....................................###.....................................|"
        ; "|.................--+--...............###...............--+--.................|"
        ; "|.................|...|...............###...............|...|.................|"
        ; "|.........#.......D...D.................................D...D......#..........|"
        ; "|.................|...|.................................|...|.................|"
        ; "|.................--+--..............--+--..............--+--.................|"
        ; "|...................#..............--|...|--..............#...................|"
        ; "|##################################D.......D##################################|"
        ; "|<..................#..............|.......|..............#..................>|"
        ; "|##################################D.......D##################################|"
        ; "|...................#..............--|...|--..............#...................|"
        ; "|.................--+--..............--+--..............--+--.................|"
        ; "|.................|...|.................................|...|.................|"
        ; "|.........#.......D...D.................................D...D......#..........|"
        ; "|.................|...|...............###...............|...|.................|"
        ; "|.................--+--...............###...............--+--.................|"
        ; "|.....................................###.....................................|"
        ; "|.............................................................................|"
        ; "-------------------------------------------------------------------------------"
        ]
        |> Map.ofPicture
        |> Map.setItems [ Item.rnComestible (); ] pavilionNW
        |> Map.setItems [ Item.rnComestible (); ] pavilionNE
        |> Map.setItems [ Item.rnComestible (); ] pavilionSE
        |> Map.setItems [ Item.rnComestible (); ] pavilionSW
        |> Map.setItems [ Item.rnPotion (); Item.runedBroadsword ] center
        |> Map.setOccupant occupant center
    in
    let rooms = [ Map.roomMax ] in
    S.{ rooms; map; level_t = Garden mitras.id }


let bigroomRooms =
    let open Map in
    let roomNW =
        { posNW = { row = 1; col = 1 }
        ; posSE = { row = center.row - 1; col = center.col - 1 }
        ; doors = []
        ; room_t = Regular
        ; lit = true
        }
    in
    let roomNE =
        { posNW = { row = 1; col = center.col + 1 }
        ; posSE = { row = center.row - 1; col = Map.size.cols - 2 }
        ; doors = []
        ; room_t = Regular
        ; lit = true
        }
    in
    let roomSW =
        { posNW = { row = center.row + 1; col = 1 }
        ; posSE = { row = Map.size.rows - 2; col = center.col - 1 }
        ; doors = []
        ; room_t = Regular
        ; lit = true
        }
    in
    let roomSE =
        { posNW = { row = center.row + 1; col = center.col + 1 }
        ; posSE = { row = Map.size.rows - 2; col = Map.size.cols - 2 }
        ; doors = []
        ; room_t = Regular
        ; lit = true
        }
    in
    [ roomNW; roomNE; roomSW; roomSE ]

let bigroom =

    let room = Map.roomMax in

    let m =
        Matrix.fill Map.size Map.{ t = Floor; occupant = None; items = [] }
        |> Matrix.set Map.{ t = StairsUp; occupant = None; items = [] } room.posNW
        |> Matrix.set Map.{ t = StairsDown; occupant = None; items = [] } room.posSE
    in

    let shouldPlaceBoulder p =
        let d = P.distance2 p center in

        p <> center
        &&
        (   d <= 8
        ||  p.row = center.row
        ||  p.col = center.col
        ||  d > 100 && Random_.oneIn 20
        )
    in
    let occupant = Some (Map.Creature (Creature.mkMinotaur ())) in

    List.fold_left
        ( fun m (p : P.t) ->
            match shouldPlaceBoulder p with
            | true ->
                let t = Matrix.get m p in
                let t = Map.{ t with occupant = Some Boulder } in
                Matrix.set t p m
            | false -> m
        )
        m
        Map.allMapPositions
    |> Map.setItems [ Item.runedBroadsword ] center
    |> Map.setOccupant occupant center
