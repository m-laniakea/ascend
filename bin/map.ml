open Matrix

module C = Common
module L = List
module M = Matrix
module P = Position
module R = Random_

let size = P.{ rows = 21; cols = 80 }

type shop_t =
    | General

type shop =
    { posDoor : P.t
    ; posEntry : P.t
    ; shop_t : shop_t
    }

type room_t =
    | Regular
    | Shop of shop

type room =
    { posNW : P.t
    ; posSE : P.t
    ; doors : P.t list
    ; room_t : room_t
    ; lit : bool
    }

type stairDirection = Up | Down

type stateDoor = Closed | Open | Hidden
type hallway = HallHidden | HallRegular
type orientation = Vertical | Horizontal

type terrain =
    | Door of stateDoor * orientation
    | Floor
    | Hallway of hallway
    | StairsDown
    | StairsUp
    | Stone
    | Unseen
    | Wall of orientation

type occupant = Creature of Creature.t | Player | Boulder

type tile =
    { t : terrain
    ; occupant : occupant option
    ; items : Item.t list
    }

type t = tile Matrix.t

let unseenEmpty = { t = Unseen; occupant = None; items = [] }

let getPosTerrain m t =
    M.mapIFindAll (fun _ p t' -> if t'.t = t then Some p else None) m
    |> List.hd

let north (pos : P.t) = {pos with row = pos.row - 1}
let south (pos : P.t) = {pos with row = pos.row + 1}
let east (pos : P.t) = {pos with col = pos.col + 1}
let west (pos : P.t) = {pos with col = pos.col - 1}
let northEast pos = P.{row = pos.row - 1; col = pos.col + 1}
let northWest pos = P.{row = pos.row - 1; col = pos.col - 1}
let southEast pos = P.{row = pos.row + 1; col = pos.col + 1}
let southWest pos = P.{row = pos.row + 1; col = pos.col - 1}

let getOutliningRoom room =
    { room with posNW = northWest room.posNW
    ; posSE = southEast room.posSE
    }

let isInMap (p : P.t) =
    p.row >= 0 && p.row < size.rows
    && p.col >= 0 && p.col < size.cols

let nextManhattan p =
    [north p; south p; east p; west p;]
    |> List.filter isInMap

let nextCorners p =
    [northEast p; northWest p; southEast p; southWest p;]
    |> List.filter isInMap

let posAround p =
    (nextManhattan p) @ (nextCorners p)

let isTerrainOf t this =
    this.t = t

let isTerrainHidden t = match t.t with
    | Door (Hidden, _) -> true
    | Hallway HallHidden -> true

    | Door _ -> false
    | Floor -> false
    | Hallway _ -> false
    | StairsDown -> false
    | StairsUp -> false
    | Stone -> false
    | Unseen -> false
    | Wall _ -> false

let isHallway t =
    t.t = Hallway HallHidden || t.t = Hallway HallRegular

let isStairs t =
    t.t = StairsUp || t.t = StairsDown

let isInRoom room (pos : P.t) =
    pos.row <= room.posSE.row
    && pos.row >= room.posNW.row
    && pos.col <= room.posSE.col
    && pos.col >= room.posNW.col

let isInShop rooms p =
    let shops = L.filter_map
        (function
        | { room_t = Shop _; _ } as r -> Some r
        | _ -> None
        )
        rooms
    in
    match shops with
        | [] -> false
        | ( {room_t = Shop s; _ } as r )::[] -> p <> s.posEntry && isInRoom r p
        | _::_ -> assert false

let randomRoomPos room =
    P.
    { row = R.rn room.posNW.row room.posSE.row
    ; col = R.rn room.posNW.col room.posSE.col
    }

let getRoomPositions room =
    let ri = C.range room.posNW.row room.posSE.row in
    let ci = C.range room.posNW.col room.posSE.col in
    List.map
    ( fun r -> List.map
        ( fun c ->
            P.{ row = r; col = c }
        ) ci
    ) ri
    |> List.flatten


let getAreaRoom room = getRoomPositions room |> L.length

let getRoomTiles room m =
    getRoomPositions room
    |> L.map (fun p -> M.get m p)

let roomHasStairs room state = L.exists isStairs (getRoomTiles room state)

let allMapPositions =
    { posNW = {row = 0; col = 0}
    ; posSE = northWest { row = size.rows; col = size.cols }
    ; doors = []
    ; room_t = Regular
    ; lit = false
    }
    |> getRoomPositions

type dirs = North | South | West | East

let getBorder room =
    let pu = room.posNW in
    let pl = room.posSE in
    let start = pu in

    (* TODO better way? *)
    (* Not as simple as using getRoomPositions, as doorGen relies on clockwise order of getBorder *)
    let rec helper acc p = function
        | South -> let n = south p in helper (n::acc) n (if n.row < pl.row then South else East)
        | East -> let n = east p in helper (n::acc) n (if n.col < pl.col then East else North)
        | North -> let n = north p in helper (n::acc) n (if n.row > pu.row then North else West)
        | West when p = start -> acc
        | West -> let n = west p in helper (n::acc) n West
    in
    helper [] start South

let isTileTypeWalkable t = match t.t with
    | Floor | StairsUp | StairsDown | Hallway HallRegular -> true
    | Door (Open, _) -> true
    | Door (Closed, _) | Door (Hidden, _) -> false
    | Hallway HallHidden -> false
    | Stone -> false
    | Unseen -> false
    | Wall Horizontal | Wall Vertical -> false

