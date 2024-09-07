open Matrix

module P = Position
module S = State

let levelGarden state =
    let mitras = Creature.mkMitras () in
    let info = { mitras.info with speed = 0 } in
    let mitras = { mitras with info } in
    let occupant = Some (Map.Creature mitras) in

    let center = Map.center in
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
    let _ = S.msgAdd state "A voice speaks to you from high and low:" in
    let _ = S.msgAdd state "\"Welcome to the Garden of Mitras." in
    let _ = S.msgAdd state "Enjoy peace among" in
    let _ = S.msgAdd state "those whos hands shed no blood\"." in
    S.{ rooms; map; level_t = Garden mitras.id }
