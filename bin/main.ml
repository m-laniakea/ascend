open Lwt
open Minttea
open Zed_string

open AStar

let cols = 80
let rows = 21

let distanceSight = 3.17

let id a = a

let range min max = List.init (max - min + 1) (fun i -> i + min)
let repeat n v = List.init n v

let contains l v = List.find_opt (fun vi -> vi = v) l |> Option.is_some

let listMake n v = List.init n (fun _ -> v)

let listMin l =
    let first = List.hd l in
    List.fold_left (fun minSofar v -> min minSofar v) first l

let listSet i v =
    List.mapi (fun ci ov -> if ci <> i then ov else v)


type pos =
    { row : int
    ; col : int
    }

type wh =
    { w : int
    ; h : int
    }

type 'a matrix =
{
    rows : int;
    cols : int;

    m : ('a list) list
}

let matrixFill rows cols v =
    { rows = rows
    ; cols = cols
    ; m = listMake rows (listMake cols v)
    }

let matrixSet v pos m =

    let mNew = List.mapi
        (fun ri r -> if ri <> pos.row then r else
            listSet pos.col v r)
        m.m
    in
    { rows = m.rows
    ; cols = m.cols
    ; m = mNew
    }

let matrixGet m pos =
    let r = List.nth m.m pos.row in
    List.nth r pos.col

let matrixGetOpt m pos =
    if pos.row < 0 || pos.col < 0 then None else
        match List.nth_opt m.m pos.row with
        | None -> None
        | Some r -> List.nth_opt r pos.col

let matrixMap f m =
    { rows = m.rows
    ; cols = m.cols
    ; m = List.map (List.map f) m.m
    }

let matrixIMap f m =
    { rows = m.rows
    ; cols = m.cols
    ; m = List.mapi
        (fun ri r ->
            List.mapi
                (fun ci c ->
                    f m { row = ri; col = ci } c
                ) r
        ) m.m
    }

let matrixFlatten m = List.flatten m.m

let matrixFold f acc m =
    matrixFlatten m
    |> List.fold_left f acc

let getPos m v =
    matrixIMap (fun _ p v' -> if v = v' then Some p else None) m
    |> matrixFlatten
    |> List.find_map id
    |> Option.get

type room =
    { posNW : pos
    ; posSE : pos
    ; doors : pos list
    }

type stateDoor = Closed | Open | Hidden
type terrain =
    | Stone
    | Hallway
    | Floor
    | StairsUp
    | StairsDown
    | Unseen
    | Door of stateDoor

type creature =
    { symbol : string (* TODO actual char *)
    ; cHp : int
    }

type occupant = Creature of creature | Boulder

type tile =
    { t : terrain
    ; occupant : occupant option
    }

type levels = tile matrix list
type stateLevels =
    { indexLevel : int
    ; levels : levels
    }

type statePlayer =
    { pos : pos
    ; hp : int
    ; hpMax : int
    ; knowledgeLevels : levels
    }

type state =
    { stateLevels : stateLevels
    ;statePlayer :statePlayer
    ; text : string
        (* objects : list Object *)
    }

let unseenEmpty = { t = Unseen; occupant = None }

let getPosTerrain m t =
    matrixIMap (fun _ p t' -> if t'.t = t then Some p else None) m
    |> matrixFlatten
    |> List.find_map id
    |> Option.get

let north pos = {pos with row = pos.row - 1}
let south pos = {pos with row = pos.row + 1}
let east pos = {pos with col = pos.col + 1}
let west pos = {pos with col = pos.col - 1}
let northEast pos = {row = pos.row - 1; col = pos.col + 1}
let northWest pos = {row = pos.row - 1; col = pos.col - 1}
let southEast pos = {row = pos.row + 1; col = pos.col + 1}
let southWest pos = {row = pos.row + 1; col = pos.col - 1}

let atNorth m pos = north pos |> matrixGetOpt m
let atSouth m pos = south pos |> matrixGetOpt m
let atEast m pos = east pos |> matrixGetOpt m
let atWest m pos = west pos |> matrixGetOpt m
let atNorthEast m pos = northEast pos |> matrixGetOpt m
let atNorthWest m pos = northWest pos |> matrixGetOpt m
let atSouthEast m pos = southEast pos |> matrixGetOpt m
let atSouthWest m pos = southWest pos |> matrixGetOpt m


let getOutline room =
    { room with posNW =
        { row = room.posNW.row - 1
        ; col = room.posNW.col - 1
        }
    ; posSE =
        { row = room.posSE.row + 1
        ; col = room.posSE.col + 1
        }
    }

let isInMap p =
    p.row >= 0 && p.row < rows
    && p.col >= 0 && p.col < cols

let nextManhattan p =
    [north p; south p; east p; west p;]
    |> List.filter isInMap

let nextCorners p =
    [northEast p; northWest p; southEast p; southWest p;]
    |> List.filter isInMap

let posAround p =
    (nextManhattan p) @ (nextCorners p)

let getSurrounding m p =
    posAround p
    |> List.map (fun n -> matrixGet m n)

let isTerrainOf t this =
    this.t = t

let hasAround m p v =
    getSurrounding m p
    |> List.exists (fun t -> t = v)

let hasAroundTerrain m p t =
    getSurrounding m p
    |> List.exists (isTerrainOf t)

let getCurrentLevel state =
    let sl = state.stateLevels in
    List.nth sl.levels sl.indexLevel

let getCurrentLevelKnowledge state =
    let sl = state.stateLevels in
    List.nth state.statePlayer.knowledgeLevels sl.indexLevel

let setCurrentLevel m state =
    let sl = state.stateLevels in
    let nl = listSet sl.indexLevel m sl.levels in
    let sln = {sl with levels = nl} in
    { state with stateLevels = sln }

let setCurrentLevelKnowledge m state =
    let sl = state.stateLevels in
    let i = sl.indexLevel in
    let pk = state.statePlayer.knowledgeLevels in
    let knowledgeLevels = listSet i m pk in
    let statePlayer = { state.statePlayer with knowledgeLevels} in
    { state with statePlayer }

let setIndexLevel i state =
    let sl = state.stateLevels in

    assert (i >= 0);
    assert (i < List.length sl.levels);

    let sl' = { sl with indexLevel = i } in
    { state with stateLevels = sl' }


let addLevel m state =
    let sl = state.stateLevels in
    let sln =
        { levels = sl.levels @ [m]
        ; indexLevel = sl.indexLevel + 1
        }
    in
    { state with stateLevels = sln }


let distance b a =
    let dr = b.row - a.row in
    let dc = b.col - a.col in
    dr * dr + dc * dc |> Float.of_int |> sqrt

let distanceManhattan f t =
    abs (t.row - f.row) + abs (t.col - f.col)

let getPathRay b a =
    let rec aux path =
        let h = List.hd path in

        if h = b then
            List.rev path
        else

        let nexts = posAround h in
        let distances = List.map (fun p -> distance b p) nexts in
        let minDist = listMin distances in
        let next = List.find (fun p -> distance b p = minDist) nexts in

        aux (next::path)
    in
    aux [a]


let isLit p = false (* TODO *)

let playerCanSee state p =
    let a = state.statePlayer.pos in
    let d = distance a p in
    if d > distanceSight && not (isLit p) then
        false
    else
    let pathTo = getPathRay p a in
    let m = getCurrentLevel state in
    let rec aux = function
        | [] -> true
        | h :: [] -> true
        | h::t -> match (matrixGet m h).t with
            | Floor | Hallway
            | StairsDown | StairsUp
            | Door Open -> aux t
            | _ -> false
    in
    aux pathTo


let playerAddHp n state =
    let sp = state.statePlayer in
    let hp = sp.hp in
    let hp' = min (hp + n) sp.hpMax |> max 0 in
    let statePlayer = { sp with hp = hp' } in
    { state with statePlayer }

let playerAddMapKnowledgeEmpty state =
    let knowledgeEmpty = matrixFill rows cols unseenEmpty in
    let knowledgeLevels = state.statePlayer.knowledgeLevels @ [knowledgeEmpty] in
    let statePlayer = { state.statePlayer with knowledgeLevels } in
    { state with statePlayer }

let playerUpdateMapKnowledge state =
    let m  = getCurrentLevel state in
    let pk = getCurrentLevelKnowledge state in
    let newVisible = matrixIMap
        ( fun _ p v ->
            if matrixGet m p <> v then
                if playerCanSee state p then
                    Some p
                else
                    None
            else
                None
        ) pk
    in
    let pk' = matrixFold
        ( fun m' p -> match p with
            | None -> m'
            | Some p ->
                let newTile = matrixGet m p in
                matrixSet newTile p m'
        ) pk newVisible
    in
    setCurrentLevelKnowledge pk' state

let playerKnowledgeDeleteCreatures state =
    let pk = getCurrentLevelKnowledge state in
    let pk' = matrixIMap
        ( fun _ p v -> match v with
            | { occupant = Some (Creature _); _ } ->
                { v with occupant = None }
            | _ -> v
        ) pk
    in
    setCurrentLevelKnowledge pk' state


let rn min max = Random.int_in_range ~min ~max

let rnItem l =
    let iLast = List.length l - 1 in
    List.nth l (rn 0 iLast)

let isInRoom pos room =
    pos.row <= room.posSE.row
    && pos.row >= room.posNW.row
    && pos.col <= room.posSE.col
    && pos.col >= room.posNW.col

let randomRoomPos room =
    { row = rn room.posNW.row room.posSE.row
    ; col = rn room.posNW.col room.posSE.col
    }

let getRoomPositions room =
    let ri = range room.posNW.row room.posSE.row in
    let ci = range room.posNW.col room.posSE.col in
    List.map
    ( fun r -> List.map
        ( fun c ->
            { row = r; col = c }
        ) ci
    ) ri
    |> List.flatten

let roomMake pu pl =
    assert (pu.row < pl.row);
    assert (pu.col < pl.col);
    { posNW = pu; posSE = pl; doors = []; }

let roomMakeWh p wh =
    assert (wh.w > 0);
    assert (wh.h > 0);

    let pl = { row = p.row + wh.h - 1; col = p.col + wh.w - 1 } in
    roomMake p pl


let doRoomsOverlap r1 r2 =
    not ( r1.posNW.col > r2.posSE.col + 3
    || r2.posNW.col > r1.posSE.col + 3
    || r1.posNW.row > r2.posSE.row + 3
    || r2.posNW.row > r1.posSE.row + 3
    )

let roomCanPlace rooms room =
    (* leave some room from edge for hallways *)
    if room.posNW.row <= 1 || room.posNW.col <= 1 || room.posSE.row >= rows - 2 || room.posSE.col >= cols - 2
    then
        false
    else
        not (List.exists (doRoomsOverlap room) rooms)


let rec roomPlace rooms wh tries =
    if tries <= 0 then None else
    let row = rn 0 (rows - 2) in
    let col = rn 0 (cols - 2) in
    let room = roomMakeWh { row = row; col = col } wh in
    if roomCanPlace rooms room then Some room else
    roomPlace rooms wh (tries - 1)

let placeCreature ~room state =
    let pp = state.statePlayer.pos in
    let positionsOk = matrixIMap
        ( fun _ p t -> match t with
            | { occupant = None; _ } ->
                ( match t.t with
                    | Floor | Hallway | StairsDown
                    | Door Open -> Some p
                    | Door Closed | Door Hidden -> None
                    | Stone | StairsUp | Unseen -> None
                )
            | _ -> None
        ) (getCurrentLevel state)
        |> matrixFlatten
        |> List.filter (fun v -> Option.is_some v && v <> Some pp)
        |> List.map Option.get
    in
    let positionsOutOfView = positionsOk |> List.filter
        ( fun p -> not (playerCanSee state p) ) in

    let creaturePos = match room with
        | Some r ->
            let rp = getRoomPositions r in
            let rpOk = positionsOk |> List.filter (contains rp) in
            let rpOutOfView = positionsOutOfView |> List.filter (contains rp) in
            ( match (rpOk, rpOutOfView) with
                | ([], []) -> None
                | (_, (h::_ as oov)) -> Some (rnItem oov)
                | (rpOk, _) -> Some (rnItem rpOk)
            )

        | None -> match (positionsOk, positionsOutOfView) with
            | ([], []) -> None
            | (_, (h::_ as oov)) -> Some (rnItem oov)
            | (rpOk, _) -> Some (rnItem rpOk)
    in
    match creaturePos with
        | None -> state
        | Some p ->
            let creature =
                { symbol = "D" (* TODO pick creatures *)
                ; cHp = 7
                }
            in
            let map = getCurrentLevel state in
            let t = matrixGet map p in
            let t' = { t with occupant = Some (Creature creature) } in
            let map' = matrixSet t' p map in
            setCurrentLevel map' state


type dirs = North | South | West | East

let removeCorners room border =
    let pu = room.posNW in
    let pl = room.posSE in
    let toRemove = [pu; pl; {row = pu.row; col = pl.col}; {row = pl.row; col = pu.col}] in
    List.filter ( fun b -> not (contains toRemove b) ) border

let removeDoorsAdjacent room border =
    let iLast = (List.length border) - 1 in
    let wrap i = if i < 0 then iLast else if i > iLast then 0 else i in
    let is = List.map (fun d -> List.find_index (fun b -> b = d) border |> Option.get) room.doors in
    let adjacent = List.map
        ( fun i ->
            [ List.nth border (wrap (i - 1))
            ; List.nth border (wrap (i + 1))
            ]
        ) is |> List.flatten
    in
    List.filter ( fun b -> not (contains adjacent b) ) border


let getBorder room =
    let pu = room.posNW in
    let pl = room.posSE in
    let start = pu in

    let rec helper acc p = function
        | South -> let n = { p with row = p.row + 1 } in helper (n::acc) n (if n.row < pl.row then South else East)
        | East -> let n = { p with col = p.col + 1 } in helper (n::acc) n (if n.col < pl.col then East else North)
        | North -> let n = { p with row = p.row - 1 } in helper (n::acc) n (if n.row > pu.row then North else West)
        | West when p = start -> acc
        | West -> let n = { p with col = p.col - 1 } in helper (n::acc) n West


    in
    helper [] start South



let rec doorGen room count =
    if count <= 0 then room else
    let outline = getOutline room in
    let border = getBorder outline |> removeCorners outline |> removeDoorsAdjacent room in
    let borderWithoutDoors =
        List.filter
            ( fun b -> contains room.doors b |> not ) border in
    let iLast = (List.length borderWithoutDoors) - 1 in
    let nd = List.nth borderWithoutDoors (rn 0 iLast) in

    doorGen { room with doors = nd::room.doors } (count - 1)

let doorsGen rooms =
    let iLast = (List.length rooms) - 1 in
    let count i = if 0 = i || iLast = i then 1 else 2 in
    List.mapi (fun i r -> doorGen r (count i)) rooms


let rec roomGen rooms tries =
    if tries <= 0 then None else
    let w = rn 3 15 in
    let h = rn 2 8 in
    match roomPlace rooms { w = w; h = h } 42 with
    | None -> roomGen rooms (tries - 1)
    | sr -> sr

let roomsGen () =
    let roomsMax = 6 in
    let rec helper sofar =
        if List.length sofar >= roomsMax
        then
            sofar
        else
        match roomGen sofar 8 with
            | None -> sofar
            | Some r -> helper (r::sofar)
    in
    helper []
    |> List.sort (fun r1 r2 -> Int.compare r1.posNW.col r2.posNW.col)
    |> doorsGen



let get_next_states pStart pGoal ?(manhattan=true) ~allowHallway ~ignoreOccupants field p =
    (
    if manhattan then
      nextManhattan p
    else
        posAround p
    )
  |> List.filter
        ( fun p -> let t =  (matrixGet field p) in
            if not ignoreOccupants && pStart <> p && Option.is_some t.occupant then
                false
            else match t.t with
            | Hallway -> allowHallway
            | Stone when hasAroundTerrain field p Floor -> false
            | Stone when hasAroundTerrain field p StairsUp -> false
            | Stone when hasAroundTerrain field p StairsDown -> false
            | Stone -> ignoreOccupants
            | Door Open  -> true
            | Door Closed | Door Hidden -> ignoreOccupants
            | Floor -> not ignoreOccupants
            | StairsUp -> not ignoreOccupants
            | StairsDown -> not ignoreOccupants
            | Unseen -> false
        )

let pToString p = "(" ^ (Int.to_string p.row) ^ ", " ^ (Int.to_string p.col) ^ ")"

(*
let bfs map start goal =
    let unVisited = matrixMap (fun _ -> true) map in

    let rec aux uv = function
        | [] -> []
        | path::otherPaths ->
            if List.hd path = goal then
                path
            else
            let pathsN = nextManhattan (List.hd path)
                    |> List.filter (fun n -> matrixGet uv n = Some true)
                    |> List.filter
                        (fun n -> if goal = n then true else match matrixGet map n with
                            | Some { t = Hallway }
                            | Some { t = Stone } when hasAround map n { t = Floor } -> false
                            | Some { t = Stone } -> true
                            | _ -> false
                        )
                    |> List.map (fun n -> n::path)
            in
            let uvn = List.fold_right (fun (h::t) uv -> matrixSet false h uv) pathsN uv in
            aux uvn (otherPaths @ pathsN)
    in
    aux unVisited [[start]]
*)

let solve field start goal =
  let open Astar in
    let cost = distanceManhattan in
    let problemWithoutHallways = { cost; goal; get_next_states = get_next_states start goal ~allowHallway:false ~ignoreOccupants:true field; } in
    let problem = { cost; goal; get_next_states = get_next_states start goal ~allowHallway:true ~ignoreOccupants:true field; } in
    match search problemWithoutHallways start with
    | None -> search problem start |> Option.get
    | Some p -> p

let isStairs t =
    t.t = StairsUp || t.t = StairsDown

let isFloorOrStairs t =
    t.t = Floor || isStairs t

let isFloorOrStairsOpt = function
    | None -> false
    | Some t -> isFloorOrStairs t

let rec charOfTerrain m pos t =
    if Option.is_some t.occupant then
        match Option.get t.occupant with
            | Creature c -> c.symbol
            | Boulder -> "0"
    else
    match t.t with
    | Stone when atNorth m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atSouth m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atEast m pos |> isFloorOrStairsOpt -> "|"
    | Stone when atWest m pos |> isFloorOrStairsOpt -> "|"
    | Stone when atSouthEast m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atSouthWest m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atSouth m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atNorthEast m pos |> isFloorOrStairsOpt -> "-"
    | Stone when atNorthWest m pos |> isFloorOrStairsOpt -> "-"
    | Stone -> " "
    | Unseen -> " "
    | Hallway -> "#"
    | Floor -> "."
    | Door Closed -> "+"
    | Door Open when atNorth m pos |> isFloorOrStairsOpt -> "|"
    | Door Open when atSouth m pos |> isFloorOrStairsOpt -> "|"
    | Door Open -> "-"
    | Door Hidden -> "*" (* TODO charOfTerrain m pos { t = Stone } *)
    | StairsUp -> "<"
    | StairsDown -> ">"

let canMoveTo t = match t.t with
    | Floor | StairsUp | StairsDown | Hallway -> true
    | Door Open -> true
    | Door _ -> false
    | Stone -> false
    | Unseen -> false


type playerActions = MoveDelta of (int * int) | Search

let creatureAddHp n t p c state =
    let cl = getCurrentLevel state in
    let t' =
        if c.cHp + n < 0 then
            (* TODO drops *)
            { t with occupant = None }
        else
            let c' = Creature { c with cHp = c.cHp + n } in
            { t with occupant = Some c' }
    in
    let cl' = matrixSet t' p cl in
    setCurrentLevel cl' state


let playerAttackMelee t p c state =
    (* TODO base on stats *)
    creatureAddHp (-5) t p c state

let playerMove (r, c) s =
    let p = s.statePlayer.pos in
    let t = getCurrentLevel s in
    let pn = { row = p.row + r; col = p.col + c } in

    match matrixGet t pn with
    | { t = Door Closed } ->
        (* TODO make chance based on stats *)
        if rn 0 2 = 0 then
            s
        else
            let tn = matrixSet
                { t = Door Open
                ; occupant = None
                }
                pn t
            in

            setCurrentLevel tn s
    | { occupant = Some (Creature c); _ } as t ->
        playerAttackMelee t pn c s

    | t_at ->
        let pn = { s.statePlayer with pos = if canMoveTo t_at then pn else p } in
        { s with statePlayer = pn }

let playerSearch model =
    (* TODO base search success on stats *)
    let currentLevel = getCurrentLevel model in
    let hiddenDoorsAround =
        posAround model.statePlayer.pos
        |> List.filter (fun pa -> matrixGet currentLevel pa |> isTerrainOf (Door Hidden) )
    in
    let terrain' = List.fold_right
        ( fun d m ->
            if (rn 0 3 > 0) then
                m
            else
                matrixSet
                    { t = Door Closed
                    ; occupant = None
                    }
                    d m
        ) hiddenDoorsAround currentLevel
    in

    setCurrentLevel terrain' model

let moveCreature a b state =
    let level = getCurrentLevel state in
    let ct = matrixGet level a in
    let tt = matrixGet level b in
    if Option.is_some tt.occupant then
        state
    else
    let level' = matrixSet { ct with occupant = None } a level in
    let level'' = matrixSet { tt with occupant = ct.occupant } b level' in
    setCurrentLevel level'' state

let getCreaturePath m start goal =
  let open Astar in
    let problem =
        { cost = distanceManhattan
        ; goal
        ; get_next_states = get_next_states start goal ~manhattan:false ~allowHallway:true ~ignoreOccupants:false m
        }
    in
    search problem start

let creatureAttackMelee p state =
    if p = state.statePlayer.pos then
        (* TODO base damage on creature/stats *)
        playerAddHp (-5) state

    else
        (* TODO allow attacking other creatures *)
        state

let animateCreature p state =
    let pp = state.statePlayer.pos in
    let cl = getCurrentLevel state in
    if distance p pp <= 1.5 then
        creatureAttackMelee pp state
    else
        match getCreaturePath cl p pp with
        | None -> state
        | Some path when List.length path <= 2 -> state
        | Some path ->
            let h = path
                |> List.tl
                |> List.rev
                |> List.tl
                |> List.hd
            in
            moveCreature p h state

let animateCreatures state =
    let creaturePositions = matrixIMap
        (* TODO refactor *)
        ( fun _ p t -> match t with
            | { occupant = Some (Creature _); _ } -> Some p
            | _ -> None
        ) (getCurrentLevel state)
        |> matrixFlatten
        |> List.filter Option.is_some
        |> List.map Option.get
    in
    List.fold_left (fun s p -> animateCreature p s) state creaturePositions

let maybeAddCreature state =
    if rn 0 50 > 0 then state else
    placeCreature ~room:None state

let playerCheckHp (state, c) =
    let sp = state.statePlayer in
    if sp.hp <= 0 then
        let _ = Format.printf "You died...\n" in
        (state, Command.Quit)
    else
        state, c

let playerAction a state =
    let s' = match a with
        | MoveDelta d -> playerMove d state
        | Search -> playerSearch state
    in
    ( animateCreatures s'
        |> maybeAddCreature
        |> playerKnowledgeDeleteCreatures
        |> playerUpdateMapKnowledge
        |> playerAddHp (if rn 0 2 = 0 then 1 else 0)
    , Command.Noop
    )
    |> playerCheckHp

let terrainAddRoom m room =
    let rp = getRoomPositions room in
    let with_floor = List.fold_right
        ( fun p m ->
            matrixSet
                { t = Floor
                ; occupant = None
                }
                p m
        ) rp m
    in
    (* TODO add Wall type *)
    List.fold_right
        ( fun d m ->
            let stateDoor = match rn 0 2 with
                | 0 -> Hidden
                | 1 -> Closed
                | 2 -> Open
                | _ -> assert false
            in
            matrixSet
                { t = Door stateDoor
                ; occupant = None
                }
                d m
        ) room.doors with_floor

let terrainAddRooms rooms t =
    List.fold_right (fun r m -> terrainAddRoom m r) rooms t

let terrainAddHallways rooms m =
    let allDoors = List.map (fun r -> r.doors) rooms |> List.concat in

    let rec aux m = function
        | [] -> m
        | d::[] -> m
        | d1::d2::t ->
            let path = solve m d1 d2
                (* remove door positions *)
                |> List.tl
                |> List.rev
                |> List.tl
            in
            let m = List.fold_right
                ( fun p m ->
                    matrixSet
                        { t = Hallway
                        ; occupant = None
                        }
                        p m
                ) path m in
            aux m t
    in
    aux m allDoors

type stairDirection = Up | Down

let rec terrainAddStairs ~dir rooms m =
    let stairType = match dir with
        | Up -> StairsUp
        | Down -> StairsDown
    in
    let stairs = { t = stairType; occupant = None } in
    match rooms with
        | [] -> assert false
        | rooms ->
            let r = rnItem rooms in
            let p = randomRoomPos r in
            if matrixGet m p |> isStairs then
                terrainAddStairs ~dir rooms m
            else
                matrixSet stairs p m

let rec placeCreatures rooms state =
    match rooms with
        | [] -> state
        | r::ro ->
            let s' = placeCreature ~room:(Some r) state in
            placeCreatures ro s'

let playerMoveToStairs ~dir model =
    let stairType = match dir with
        | Up -> StairsUp
        | Down -> StairsDown
    in
    let posStairs = getPosTerrain (getCurrentLevel model) stairType
    in
    let statePlayer = { model.statePlayer with pos = posStairs } in
    { model with statePlayer }


let mapGen model =
    let rooms = roomsGen () in
    let terrain = matrixFill rows cols { t = Stone; occupant = None }
        |> terrainAddRooms rooms
        |> terrainAddStairs ~dir:Up rooms
        |> terrainAddStairs ~dir:Down rooms
        |> terrainAddHallways rooms
    in
    addLevel terrain model
    |> playerMoveToStairs ~dir:Up
    |> placeCreatures rooms


let playerGoUp model =
    let p = model.statePlayer.pos in
    if matrixGet (getCurrentLevel model) p |> isTerrainOf StairsUp |> not then
        (model, Command.Noop)
    else
        let sl = model.stateLevels in
        if sl.indexLevel = 0 then
            (model, Command.Noop)
        else
            let s' = setIndexLevel (sl.indexLevel - 1) model
                |> playerMoveToStairs ~dir:Down
            in
            (s', Command.Noop)

let playerGoDown model =
    let p = model.statePlayer.pos in
    if matrixGet (getCurrentLevel model) p |> isTerrainOf StairsDown |> not then
        (model, Command.Noop)
    else
        let sl = model.stateLevels in
        if sl.indexLevel = List.length sl.levels - 1 then
            ( mapGen model
                |> playerMoveToStairs ~dir:Up
                |> playerAddMapKnowledgeEmpty
                |> playerUpdateMapKnowledge
            , Command.Noop
            )
        else
            let s' = setIndexLevel (sl.indexLevel + 1) model
                |> playerMoveToStairs ~dir:Up
            in
            (s', Command.Noop)


let init _model = Command.Hide_cursor

let update event model = match event with
    | Event.KeyDown (Key "q" | Escape) -> (model, Command.Quit)
    | Event.KeyDown (Key "h") -> playerAction (MoveDelta (0, -1)) model
    | Event.KeyDown (Key "l") -> playerAction (MoveDelta (0,  1)) model
    | Event.KeyDown (Key "k") -> playerAction (MoveDelta (-1, 0)) model
    | Event.KeyDown (Key "j") -> playerAction (MoveDelta (1,  0)) model

    | Event.KeyDown (Key "y") -> playerAction (MoveDelta (-1, -1)) model
    | Event.KeyDown (Key "u") -> playerAction (MoveDelta (-1,  1)) model
    | Event.KeyDown (Key "b") -> playerAction (MoveDelta (1,  -1)) model
    | Event.KeyDown (Key "n") -> playerAction (MoveDelta (1,   1)) model

    | Event.KeyDown (Key "s") -> playerAction Search model

    | Event.KeyDown (Key "<") -> playerGoUp model
    | Event.KeyDown (Key ">") -> playerGoDown model
    | _ -> (model, Command.Noop)

let view model =
    let p = model.statePlayer.pos in
    let m = getCurrentLevelKnowledge model in
    let a = matrixIMap charOfTerrain m in
    let a2 = matrixSet "@" p a in
    let b = List.map (String.concat "") a2.m in
    let s = String.concat "\n" b in
    Format.sprintf
{| HP: %i

%s|} model.statePlayer.hp s


let initial_model =
    Random.init 0;

    let stateLevels =
        { indexLevel = -1
        ; levels = []
        }
    in

    let statePlayer =
        { pos = { row = 0; col = 0 }
        ; hp = 613
        ; hpMax = 613
        ; knowledgeLevels = []
        }
    in

    let stateI =
        { stateLevels
        ; statePlayer
        ; text = ""
        }
    in
    mapGen stateI
    |> playerMoveToStairs ~dir:Up
    |> playerAddMapKnowledgeEmpty
    |> playerUpdateMapKnowledge

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start app ~initial_model
