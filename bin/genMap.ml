open Matrix

module L = List

module C = Common
module P = Position
module R = Random_
module SL = StateLevels

let getHallwayPointNext pGoal ~allowHallway (m : Map.t) p =
    Map.nextManhattan p
    |> List.filter
        ( fun p ->
            match (Matrix.get m p).t with
            | Door _ -> true
            | Floor -> false
            | Hallway _ -> allowHallway
            | StairsDown -> false
            | StairsUp -> false
            | Stone -> true
            | Unseen -> false
            | Wall Horizontal | Wall Vertical -> false
        )

let getPathHallway m start goal =
  let open AStar in
  let open AStar in
    let cost = P.distanceManhattan in
    let problemWithoutHallways = { cost; goal; get_next_states = getHallwayPointNext goal ~allowHallway:false m; } in
    let problem = { cost; goal; get_next_states = getHallwayPointNext goal ~allowHallway:true m; } in
    match search problemWithoutHallways start with
    | None -> search problem start |> Option.get
    | Some p -> p

let roomMake (pu : P.t) (pl : P.t) state =
    assert (pu.row < pl.row);
    assert (pu.col < pl.col);
    Map.
    { posNW = pu
    ; posSE = pl
    ; doors = []
    ; room_t = Regular
    ; lit = SL.depthNext state < 10 && not (R.oneIn 50)
    }

let roomMakeFromSize p (size : P.dim_2D) state =
    assert (size.rows > 0);
    assert (size.cols > 0);

    let pl = P.{ row = p.row + size.rows - 1; col = p.col + size.cols - 1 } in
    roomMake p pl state

let doRoomsOverlap (r1 : Map.room) (r2 : Map.room) =
    not ( r1.posNW.col > r2.posSE.col + 3
    || r2.posNW.col > r1.posSE.col + 3
    || r1.posNW.row > r2.posSE.row + 3
    || r2.posNW.row > r1.posSE.row + 3
    )

let roomCanPlace rooms (room : Map.room) =
    (* leave some room from edge for hallways *)
    if room.posNW.row < 2 || room.posNW.col < 2
        || room.posSE.row >= Map.size.rows - 2
        || room.posSE.col >= Map.size.cols - 2
    then
        false
    else
        not (List.exists (doRoomsOverlap room) rooms)

let rec roomPlace rooms size tries state =
    if tries <= 0 then None else
    let row = R.rn 2 (Map.size.rows - 2) in
    let col = R.rn 2 (Map.size.cols - 2) in
    let room = roomMakeFromSize { row = row; col = col } size state in
    if roomCanPlace rooms room then Some room else
    roomPlace rooms size (tries - 1) state


let removeCorners (room : Map.room) border =
    let pu = room.posNW in
    let pl = room.posSE in
    let toRemove = [pu; pl; {row = pu.row; col = pl.col}; {row = pl.row; col = pu.col}] in
    List.filter ( fun b -> not (C.contains toRemove b) ) border

let removeDoorsAdjacent (room : Map.room) outline =
    let iLast = (List.length outline) - 1 in
    let wrap i = if i < 0 then iLast else if i > iLast then 0 else i in
    let iDoors = List.map (fun d -> List.find_index (fun b -> b = d) outline |> Option.get) room.doors in
    let adjacent = List.map
        ( fun i ->
            [ List.nth outline (wrap (i - 1))
            ; List.nth outline (wrap (i + 1))
            ]
        ) iDoors |> List.flatten
    in
    List.filter ( fun b -> not (C.contains adjacent b) ) outline

let rec doorGen room count =
    if count <= 0 then room else
    let outlineR = Map.getOutliningRoom room in
    let positionsValid = Map.getBorder outlineR |> removeCorners outlineR |> removeDoorsAdjacent room in
    let pValidNoDoors =
        List.filter
            ( fun b -> C.contains room.doors b |> not ) positionsValid in
    let doorNew = R.item pValidNoDoors in

    doorGen { room with doors = doorNew::room.doors } (count - 1)

let doorsGen rooms =
    let iLast = (List.length rooms) - 1 in
    let count i = if 0 = i || iLast = i then 1 else 2 in
    List.mapi (fun i r -> doorGen r (count i)) rooms

let rec roomGen rooms tries state =
    if tries <= 0 then None else
    let w = R.rn 2 13 in
    let h = R.rn 2 8 in
    match roomPlace rooms { cols = w; rows = h } 42 state with
    | None -> roomGen rooms (tries - 1) state
    | sr -> sr

let isSuitableForShop m (room : Map.room) =
    1 = L.length room.doors
    && not (Map.roomHasStairs room m)
    && Map.getAreaRoom room <= 24
    && Map.getAreaRoom room >= 4

let maybeMakeShop rooms state m =
    let d = SL.depthNext state in
    if d <= 1 || (R.rn 0 d) >= 3 then rooms else

    let indices = L.mapi (fun i _ -> i) rooms in
    let ixsRoomOk = L.filter (fun i -> L.nth rooms i |> isSuitableForShop m) indices in
    if L.is_empty ixsRoomOk then rooms else

    let i = R.item ixsRoomOk in
    let room = L.nth rooms i in

    let posDoor = List.hd room.doors in
    let posEntry =
        let open Map in
        match posDoor with
        | p when isInRoom room (north p) -> north p
        | p when isInRoom room (east p) -> east p
        | p when isInRoom room (south p) -> south p
        | p when isInRoom room (west p) -> west p
        | _ -> assert false
    in
    let shop =
        Map.
        { posDoor
        ; posEntry
        ; shop_t = General
        }
    in
    let room = { room with room_t = Shop shop } in
    let rooms = C.listSet i room rooms in

    rooms

let roomsGen state =
    let roomsMax = R.rn 4 7 in
    let rec helper sofar =
        if List.length sofar >= roomsMax
        then
            sofar
        else
        match roomGen sofar 8 state with
            | None -> sofar
            | Some r -> helper (r::sofar)
    in
    helper []
    |> List.sort (fun (r1 : Map.room) r2 -> Int.compare r1.posNW.col r2.posNW.col)
    |> doorsGen

let terrainAddRoom m room =
    let rp = Map.getRoomPositions room in
    let withFloor = List.fold_left
        ( fun m p ->
            Matrix.set
                Map.
                { t = Floor
                ; occupant = None
                ; items = []
                }
                p m
        ) m rp
    in
    let olR = Map.getOutliningRoom room in
    let outline = Map.getBorder olR in
    let withWalls = List.fold_left
        ( fun m p ->
            let alignment = match (p : P.t) with
                | _ when p.row = olR.posNW.row -> Map.Horizontal
                | _ when p.row = olR.posSE.row -> Map.Horizontal
                | _ -> Vertical
            in
            Matrix.set Map.{ t = Wall alignment; occupant = None; items = [] } p m
        )
        withFloor
        outline
    in
    List.fold_left
        ( fun m d ->
            let stateDoor = match R.rn 0 5 with
                | 0 -> Map.Hidden
                | 1 | 2 | 3 -> Closed
                | 4 | 5 -> Open
                | _ -> assert false
            in
            let ori = match Matrix.get m d with
                | Map.{ t = Wall ori; _ } -> ori
                | _ -> assert false
            in
            Matrix.set
                Map.
                { t = Door (stateDoor, ori)
                ; occupant = None
                ; items = []
                }
                d m
        ) withWalls room.doors

let terrainAddRooms rooms t =
    List.fold_right (fun r m -> terrainAddRoom m r) rooms t

let maybeAddBoulder hallway m =
    if R.nToOne 49 then m else
    let p = R.item hallway in
    let t = Matrix.get m p in
    Matrix.set Map.{ t with occupant = Some Boulder; t = Hallway HallRegular } p m

let terrainAddHallways rooms m =
    let allDoors = List.map (fun (r : Map.room) -> r.doors) rooms |> List.concat in

    let rec aux m = function
        | [] -> m
        | _::[] -> m
        | d1::d2::t ->
            let path = getPathHallway m d1 d2
                (* remove door positions *)
                |> List.tl
                |> List.rev
                |> List.tl
            in
            let m = List.fold_left
                ( fun m p ->
                    let tCurrent = Matrix.get m p in
                    if Map.isHallway tCurrent then
                        m
                    else
                        let hT = if R.oneIn 100 then Map.HallHidden else HallRegular in
                        Matrix.set
                            { tCurrent with t = Hallway hT }
                            p m
                )
                m path
            in
            aux m t
            |> maybeAddBoulder path
    in
    aux m allDoors

let rec terrainAddStairs ~dir rooms m =
    let stairType = match dir with
        | Map.Up -> Map.StairsUp
        | Map.Down -> Map.StairsDown
    in
    let stairs = Map.{ t = stairType; occupant = None; items = [] } in
    match rooms with
        | [] -> assert false
        | rooms ->
            let r = R.item rooms in
            let p = Map.randomRoomPos r in
            if Matrix.get m p |> Map.isStairs then
                terrainAddStairs ~dir rooms m
            else
                Matrix.set stairs p m

let rec placeRoomCreatures rooms state =
    match rooms with
        | [] -> state
        | r::ro ->
            let s' = if R.oneIn 3 then Ai.placeCreature ~room:(Some r) state else state in
            placeRoomCreatures ro s'

let maybeAddItem ~gold room state m =
    if R.nToOne 2 then m else
    let p = Map.randomRoomPos room in
    UpdateMap.addItem ~gold state m p

let terrainAddObjects rooms state m =
    List.fold_left
        ( fun m' (r : Map.room) -> match r.room_t with
            | Regular ->
                m'
                |> maybeAddItem ~gold:false r state
                |> maybeAddItem ~gold:true r state

            | Shop shop ->
                Map.getRoomPositions r
                |> L.filter (fun p -> p <> shop.posEntry)
                |> L.fold_left (fun m p -> UpdateMap.addItem ~gold:false state m p) m'
        )
        m
        rooms

let gen state =
    let rooms = roomsGen state in
    let terrain = Matrix.fill Map.size Map.{ t = Stone; occupant = None; items = [] }
        |> terrainAddRooms rooms
        |> terrainAddStairs ~dir:Up rooms
        |> terrainAddStairs ~dir:Down rooms
        |> terrainAddHallways rooms
    in
    let rooms = maybeMakeShop rooms state terrain in
    let map = terrainAddObjects rooms state terrain in
    SL.levelAdd { rooms; map } state
    |> Player.moveToStairs ~dir:Up
    |> placeRoomCreatures rooms


