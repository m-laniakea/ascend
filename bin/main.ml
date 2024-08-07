open Notty

open Common
open Matrix

module Cr = Creature
module L = List
module Q = Queue

module Term = Notty_unix.Term

let term = Term.create ()

let mapSize =
    { row = 21
    ; col = 80
    }

let distanceSight = 3.17

let itemsDisplayedMax = 5

let id a = a

let partitionI f l =
    L.mapi (fun i v -> i, v) l
    |> L.partition_map (fun (i, v) -> if f i v then Left v else Right v)

let listMin l =
    let first = List.hd l in
    List.fold_left (fun minSofar v -> min minSofar v) first l

let listTake n = L.filteri (fun i _ -> i < n)

let listRemove v l = match L.find_index ((=) v) l with
    | None -> l
    | Some i -> partitionI (fun ix _ -> ix <> i) l |> fst

type wh =
    { w : int
    ; h : int
    }

type shop_t =
    | General

type shop =
    { posDoor : pos
    ; posEntry : pos
    ; shop_t : shop_t
    }

type room_t =
    | Regular
    | Shop of shop

type room =
    { posNW : pos
    ; posSE : pos
    ; doors : pos list
    ; room_t : room_t
    }

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

type level =
    { map : tile Matrix.t
    ; rooms : room list
    }

type levels = level list

type stateLevels =
    { indexLevel : int
    ; levels : levels
    }

type statePlayer =
    { pos : pos
    ; gold : int
    ; hp : int
    ; hpMax : int
    ; weaponWielded : Item.weapon option
    ; inventory : Item.t list
    ; knowledgeLevels : levels
    }

type selectionItem =
    { iIndex : int
    ; name : string
    ; selected : bool
    ; letter : char
    }

type onSelectItemsComplete =
    | DoDrop
    | DoPickup
    | DoQuaff
    | DoRead
    | DoWield
    | SelectDirThrow
    | SelectDirZap

type selectDir =
    | DoThrow of selectionItem
    | DoZap of selectionItem

type selectItems =
    { sItems : selectionItem list
    ; single : bool
    ; onComplete : onSelectItemsComplete
    }

type selection =
    | SelectDir of selectDir
    | SelectItems of selectItems

type mode =
    | Playing
    | Selecting of selection

type state =
    { stateLevels : stateLevels
    ; statePlayer : statePlayer
    ; messages : string Q.t
    ; mode : mode
    ; turns : int
    (* objects : list Object *)
    }

type animation =
    { dir : pos
    ; posStart : pos
    ; posCurrent : pos
    ; posEnd : pos
    ; image : image
    }

let msgAdd state s = Q.push s state.messages

let unseenEmpty = { t = Unseen; occupant = None; items = [] }

let getPosTerrain m t =
    Matrix.mapIFindAll (fun _ p t' -> if t'.t = t then Some p else None) m
    |> List.hd

let north pos = {pos with row = pos.row - 1}
let south pos = {pos with row = pos.row + 1}
let east pos = {pos with col = pos.col + 1}
let west pos = {pos with col = pos.col - 1}
let northEast pos = {row = pos.row - 1; col = pos.col + 1}
let northWest pos = {row = pos.row - 1; col = pos.col - 1}
let southEast pos = {row = pos.row + 1; col = pos.col + 1}
let southWest pos = {row = pos.row + 1; col = pos.col - 1}

let atNorth m pos = north pos |> Matrix.getOpt m
let atSouth m pos = south pos |> Matrix.getOpt m
let atEast m pos = east pos |> Matrix.getOpt m
let atWest m pos = west pos |> Matrix.getOpt m
let atNorthEast m pos = northEast pos |> Matrix.getOpt m
let atNorthWest m pos = northWest pos |> Matrix.getOpt m
let atSouthEast m pos = southEast pos |> Matrix.getOpt m
let atSouthWest m pos = southWest pos |> Matrix.getOpt m

let getOutliningRoom room =
    { room with posNW = northWest room.posNW
    ; posSE = southEast room.posSE
    }

let isInMap p =
    p.row >= 0 && p.row < mapSize.row
    && p.col >= 0 && p.col < mapSize.col

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
    |> List.map (fun n -> Matrix.get m n)

let isTerrainOf t this =
    this.t = t

let getCurrentLevel state =
    let sl = state.stateLevels in
    List.nth sl.levels sl.indexLevel

let getCurrentMap state =
    (getCurrentLevel state).map

let getKnowledgeCurrentLevel state =
    let sl = state.stateLevels in
    List.nth state.statePlayer.knowledgeLevels sl.indexLevel

let getKnowledgeCurrentMap state =
    (getKnowledgeCurrentLevel state).map

let getDepth state =
    let sl = state.stateLevels in
    sl.indexLevel + 1

let getDepthNext state = (getDepth state) + 1

let setCurrentMap m state =
    let sl = state.stateLevels in
    let cl = getCurrentLevel state in
    let nl = listSet sl.indexLevel { cl with map = m } sl.levels in
    let sln = {sl with levels = nl} in
    { state with stateLevels = sln }

let setKnowledgeCurrentMap m state =
    let sp = state.statePlayer in
    let i = state.stateLevels.indexLevel in
    let ckl = getKnowledgeCurrentLevel state in
    let knowledgeLevels = listSet i { ckl with map = m } sp.knowledgeLevels in
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

let posAdd a b =
    { row = a.row + b.row
    ; col = a.col + b.col
    }

let posDiff a b =
    { row = b.row - a.row
    ; col = b.col - a.col
    }

let posDir p =
    (* normalizes delta. WARN: only works for lines on x or + shapes *)
    assert (p.row <> 0 || p.col <> 0);
    let den = max (abs p.row) (abs p.col) in
    { row = p.row / den
    ; col = p.col / den
    }

let dirRevCol d = { d with col = -d.col }
let dirRevRow d = { d with row = -d.row }
let dirRevBoth d = dirRevCol d |> dirRevRow

let distance b a =
    let dr = b.row - a.row in
    let dc = b.col - a.col in
    dr * dr + dc * dc |> Float.of_int |> sqrt

let distanceManhattan f t =
    abs (t.row - f.row) + abs (t.col - f.col)

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

let isFloorOrStairs t =
    t.t = Floor || isStairs t

let imageOfItem ?(styles=A.(st bold)) (i : Item.t) = match i with
    | Container _ -> I.string A.(styles ++ fg brown) "("
    | Corpse c -> I.string A.(styles ++ fg c.color) "%"
    | Gold _ -> I.string A.(styles ++ fg lightyellow) "$"
    | Potion _ -> I.string A.(styles ++ fg white) "!"
    | Scroll _ -> I.string A.(styles ++ fg white) "?"
    | Weapon w -> I.string A.(styles ++ fg w.color) ")"
    | Wand _ -> I.string A.(styles ++ fg A.cyan) "/"

let imageOfTile _ _ = function
    | { occupant = Some occ; _ } ->
        ( match occ with
            | Creature c ->
                I.string A.(st bold ++ fg c.info.color) c.info.symbol
            | Player -> I.string A.(st bold ++ fg lightwhite) "@"
            | Boulder -> I.string A.(st bold ++ fg white) "0"
        )
    | { items = topItem::others; _ } ->
        let styles = if others = [] then A.(st bold) else A.(bg lightblack ++ st bold) in
        imageOfItem ~styles topItem
    | t ->
        let c = match t.t with
        | Stone -> " "
        | Unseen -> " "
        | Hallway HallHidden -> " "
        | Hallway HallRegular -> "#"
        | Floor -> "."
        | Door (Closed, _) -> "+"
        | Door (Open, Horizontal) | Door (Hidden, Vertical) -> "|"
        | Door (Open, Vertical) | Door (Hidden, Horizontal) -> "-"
        | StairsUp -> "<"
        | StairsDown -> ">"
        | Wall Horizontal -> "-"
        | Wall Vertical -> "|"
        in
        I.string A.(fg white) c

let applyAnimatedTiles animationLayer m =
    animationLayer
    |> List.fold_left
        ( fun m (pos, styledChar) ->
            Matrix.set styledChar pos m
        )
        m

let imageCreate ?(animationLayer=[]) state =
    let open Notty.Infix in
    ( match state.mode with
        | Playing ->
            Format.sprintf "HP: %i | DLvl: %i |" state.statePlayer.hp state.stateLevels.indexLevel
            |> I.string A.empty
            <->
            (
                let mView =
                    getKnowledgeCurrentMap state
                    |> Matrix.mapI imageOfTile
                    |> applyAnimatedTiles animationLayer
                in
                I.tabulate mapSize.col mapSize.row (fun c r -> Matrix.get mView { row = r; col = c })
            )
            <-> I.string A.empty (sf "$ %i" state.statePlayer.gold)
            <|>
            (
                Q.fold (fun i m -> i <-> (I.string A.empty m)) I.empty state.messages
            )
        | Selecting s ->
                ( match s with
                | SelectDir _ ->
                    [ "y  k  u"
                    ; " \\ | / "
                    ; "h  @  l"
                    ; " / | \\"
                    ; "b  j  n"
                    ]
                    |> L.map (I.string A.empty)
                    |> I.vcat

                | SelectItems s ->
                    s.sItems
                    |> L.sort (fun s s'-> Char.compare s.letter s'.letter)
                    |> L.map (fun s -> I.string A.empty (sf "%c %s %s" s.letter (if s.selected then "+" else "-") s.name))
                    |> I.vcat
                )
    )
    |> Term.image term

let closestTo p p1 p2 = Int.compare (distanceManhattan p p1) (distanceManhattan p p2)

let rec animate state ?(cumulative=true) ?(linger=true) ?(animationLayer=[]) a =
    let animationLayer' = match cumulative with
        | _ when a.posCurrent = a.posStart -> []
        | true -> (a.posCurrent, a.image)::animationLayer
        | false -> [a.posCurrent, a.image]
    in
    imageCreate ~animationLayer:animationLayer' state;
    Unix.sleepf (if cumulative then 0.05 else 0.10);

    if a.posCurrent = a.posEnd then
        if linger then Unix.sleepf 0.3 else ()
    else
    let posCurrent = posAdd a.posCurrent a.dir in
    let a' = { a with posCurrent } in

    animate state ~cumulative ~linger ~animationLayer:animationLayer' a'

let getPathRay from target =
    let rec aux path =
        let h = List.hd path in

        if h = target then
            List.rev path
        else

        let nexts = posAround h in
        let distances = List.map (fun p -> distance target p) nexts in
        let minDist = listMin distances in
        let next = List.find (fun p -> distance target p = minDist) nexts in

        aux (next::path)
    in
    aux [from]


let isLit p = false (* TODO *)

let rec rayCanHitTarget m prev path =
    let t = Matrix.get m (List.hd path) in match path with
    | [] -> true
    | _::[] ->
        ( match t.t with
            | Wall _ when prev.t = Hallway HallRegular -> false
            | Door (Hidden, _) when prev.t = Hallway HallRegular -> false
            | _ -> true
        )
    | _::tl -> match t.occupant with
        | Some Boulder -> false
        | Some Player -> rayCanHitTarget m t tl
        | Some (Creature c) ->
            rayCanHitTarget m t tl
            (* TODO large occupants *)
        | None -> match t.t with
            | Floor | Hallway HallRegular
            | StairsDown | StairsUp
            | Door (Open, _) -> rayCanHitTarget m t tl
            | Door (Closed, _) | Door (Hidden, _) -> false
            | Hallway HallHidden -> false
            | Stone -> false
            | Unseen -> false
            | Wall _ -> false

let canSee distanceSight from toSee state =
    let d = distance from toSee in
    if d > distanceSight && not (isLit toSee) then
        false
    else
    let pathTo = getPathRay from toSee in
    let m = getCurrentMap state in
    rayCanHitTarget m (Matrix.get m from) pathTo

let playerCanSee state toSee =
    let pp = state.statePlayer.pos in
    (* TODO blindness *)
    canSee distanceSight pp toSee state

let creatureCanSee c from toSee state =
    (* TODO use creature info *)
    canSee 6.16 from toSee state

let areLinedUp a b =
    (* x or + shaped lines only *)
    let pd = posDiff a b in
    abs(pd.row) = abs(pd.col) || (pd.row = 0) <> (pd.col = 0)


let playerAddHp n state =
    let sp = state.statePlayer in
    let hp = sp.hp in
    let hp' = min (hp + n) sp.hpMax |> max 0 in
    let statePlayer = { sp with hp = hp' } in
    { state with statePlayer }

let playerAddMapKnowledgeEmpty state =
    let knowledgeEmpty = { rooms = []; map = Matrix.fill mapSize unseenEmpty } in
    let knowledgeLevels = state.statePlayer.knowledgeLevels @ [knowledgeEmpty] in
    let statePlayer = { state.statePlayer with knowledgeLevels } in
    { state with statePlayer }

let playerUpdateMapKnowledge state =
    let m  = getCurrentMap state in
    let pk = getKnowledgeCurrentMap state in
    let pkUpdated = Matrix.foldI
        ( fun _ p pk' known ->
            let actual = Matrix.get m p in
            if actual <> known && playerCanSee state p then
                Matrix.set actual p pk'
            else
                pk'
        ) pk pk
    in
    setKnowledgeCurrentMap pkUpdated state

let playerKnowledgeDeleteCreatures state =
    let pk = getKnowledgeCurrentMap state in
    let pk' = Matrix.map
        ( fun v -> match v with
            | { occupant = Some (Creature _); _ } ->
                { v with occupant = None }
            | { occupant = Some Player; _ } ->
                { v with occupant = None }
            | _ -> v
        ) pk
    in
    setKnowledgeCurrentMap pk' state

let isInRoom room pos =
    pos.row <= room.posSE.row
    && pos.row >= room.posNW.row
    && pos.col <= room.posSE.col
    && pos.col >= room.posNW.col

let playerIsInShop state =
    let sp = state.statePlayer in
    let pp = sp.pos in
    let cl = getCurrentLevel state in

    match L.filter (function | { room_t = Shop _; _ } -> true | _ -> false) cl.rooms with
        | [] -> false
        | ( {room_t = Shop s; _ } as r )::[] -> pp <> s.posEntry && isInRoom r pp
        | _::_ -> assert false

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

let getRoomArea room = getRoomPositions room |> L.length

let getRoomTiles room m =
    getRoomPositions room
    |> L.map (fun p -> Matrix.get m p)

let roomHasStairs room state = L.exists isStairs (getRoomTiles room state)

let allMapPositions =
    { posNW = {row = 0; col = 0}
    ; posSE = northWest mapSize
    ; doors = []
    ; room_t = Regular
    }
    |> getRoomPositions

let roomMake pu pl =
    assert (pu.row < pl.row);
    assert (pu.col < pl.col);
    { posNW = pu; posSE = pl; doors = []; room_t = Regular }

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
    if room.posNW.row < 2 || room.posNW.col < 2
        || room.posSE.row >= mapSize.row - 2
        || room.posSE.col >= mapSize.col - 2
    then
        false
    else
        not (List.exists (doRoomsOverlap room) rooms)


let rec roomPlace rooms wh tries =
    if tries <= 0 then None else
    let row = rn 2 (mapSize.row - 2) in
    let col = rn 2 (mapSize.col - 2) in
    let room = roomMakeWh { row = row; col = col } wh in
    if roomCanPlace rooms room then Some room else
    roomPlace rooms wh (tries - 1)

let canSpawnHere ?(forbidPos=None) m p =
    if Some p = forbidPos then
        false
    else
    match Matrix.get m p with
        (* TODO water, phasing, etc. *)
        | { occupant = Some _; _ } -> false
        | { occupant = None; t = t; _ } ->
            ( match t with
                | Door (Open, _) -> true
                | Door (Closed, _) | Door (Hidden, _) -> false
                | Floor -> true
                | Hallway HallHidden -> false
                | Hallway HallRegular -> true
                | StairsDown -> true
                | StairsUp  -> false
                | Stone -> false
                | Unseen -> false
                | Wall Horizontal | Wall Vertical -> false
            )

let findItemMatchingInSight c f from state =
    getCurrentMap state
    |> Matrix.mapIFindAll
        ( fun m p t -> match L.exists f t.items with
            | false -> None
            | true when from <> p && not (canSpawnHere m p) -> None
            | true -> Some p
        )
    |> L.filter (fun p -> creatureCanSee c from p state)
    |> L.sort (closestTo from)


let placeCreature ?(preferNearby=false) ~room state =
    let m = getCurrentMap state in
    let pp = state.statePlayer.pos in
    let spawnPositions =
        allMapPositions
        |> L.filter (fun p -> canSpawnHere ~forbidPos:(Some pp) m p)
    in
    let pInView, pOutOfView =
        spawnPositions
        |> L.partition (fun p -> playerCanSee state p)
    in

    let pInView, pOutOfView = match room with
        | None -> pInView, pOutOfView
        | Some r ->
              L.filter (isInRoom r) pInView
            , L.filter (isInRoom r) pOutOfView
    in
    let closestFirst = L.sort (closestTo pp) in
    let creaturePos =
        if preferNearby then
            L.nth_opt
            ((closestFirst pInView) @ (closestFirst pOutOfView))
            0
        else
            match pOutOfView, pInView with
            | [], [] -> None
            | (_::_ as oov), _ -> Some (rnItem oov)
            | _, pOk -> Some (rnItem pOk)
    in
    match creaturePos with
        | None -> state
        | Some p ->
            let d = getDepth state in (* TODO difficulty ob1 on level gen *)
            match Cr.random d with
                | None -> state
                | Some creature ->
                    let map = getCurrentMap state in
                    let t = Matrix.get map p in
                    let t' = { t with occupant = Some (Creature creature) } in
                    let map' = Matrix.set t' p map in
                    setCurrentMap map' state

let rec placeCreatures ?(preferNearby=false) ~room count state =
    if count <= 0 then state else
    let state = placeCreature ~preferNearby ~room state in
    placeCreatures ~preferNearby ~room (count - 1) state


type dirs = North | South | West | East

let removeCorners room border =
    let pu = room.posNW in
    let pl = room.posSE in
    let toRemove = [pu; pl; {row = pu.row; col = pl.col}; {row = pl.row; col = pu.col}] in
    List.filter ( fun b -> not (contains toRemove b) ) border

let removeDoorsAdjacent room outline =
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
    List.filter ( fun b -> not (contains adjacent b) ) outline


let getBorder room =
    let pu = room.posNW in
    let pl = room.posSE in
    let start = pu in

    (* TODO better way? *)
    (* Not as simple as using getRoomPositions, as doorGen relies on clockwise order of getBorder *)
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
    let outlineR = getOutliningRoom room in
    let positionsValid = getBorder outlineR |> removeCorners outlineR |> removeDoorsAdjacent room in
    let pValidNoDoors =
        List.filter
            ( fun b -> contains room.doors b |> not ) positionsValid in
    let doorNew = rnItem pValidNoDoors in

    doorGen { room with doors = doorNew::room.doors } (count - 1)

let doorsGen rooms =
    let iLast = (List.length rooms) - 1 in
    let count i = if 0 = i || iLast = i then 1 else 2 in
    List.mapi (fun i r -> doorGen r (count i)) rooms

let rec roomGen rooms tries =
    if tries <= 0 then None else
    let w = rn 2 13 in
    let h = rn 2 8 in
    match roomPlace rooms { w = w; h = h } 42 with
    | None -> roomGen rooms (tries - 1)
    | sr -> sr

let isSuitableForShop m room =
    1 = L.length room.doors
    && not (roomHasStairs room m)
    && getRoomArea room <= 24
    && getRoomArea room >= 4

let maybeMakeShop rooms state m =
    let d = getDepthNext state in
    if d <= 1 || (rn 0 d) >= 3 then rooms else

    let indices = L.mapi (fun i _ -> i) rooms in
    let ixsRoomOk = L.filter (fun i -> L.nth rooms i |> isSuitableForShop m) indices in
    if L.is_empty ixsRoomOk then rooms else

    let i = rnItem ixsRoomOk in
    let room = L.nth rooms i in

    let posDoor = List.hd room.doors in
    let posEntry = match posDoor with
        | p when isInRoom room (north p) -> north p
        | p when isInRoom room (east p) -> east p
        | p when isInRoom room (south p) -> south p
        | p when isInRoom room (west p) -> west p
        | _ -> assert false
    in
    let shop =
        { posDoor
        ; posEntry
        ; shop_t = General
        }
    in
    let room = { room with room_t = Shop shop } in
    let rooms = listSet i room rooms in

    rooms

let roomsGen () =
    let roomsMax = rn 4 7 in
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

let get_next_states pGoal ?(manhattan=true) ~allowHallway ~isMapGen m p =
    (
    if manhattan then
        nextManhattan p
    else
        posAround p
    )
  |> List.filter
        ( fun p ->
            if not isMapGen then
                pGoal = p || canSpawnHere m p
            else
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

(*
let bfs map start goal =
    let unVisited = Matrix.map (fun _ -> true) map in

    let rec aux uv = function
        | [] -> []
        | path::otherPaths ->
            if List.hd path = goal then
                path
            else
            let pathsN = nextManhattan (List.hd path)
                    |> List.filter (fun n -> Matrix.get uv n = Some true)
                    |> List.filter
                        (fun n -> if goal = n then true else match Matrix.get map n with
                            | Some { t = Hallway }
                            | Some { t = Stone } when hasAround map n { t = Floor } -> false
                            | Some { t = Stone } -> true
                            | _ -> false
                        )
                    |> List.map (fun n -> n::path)
            in
            let uvn = List.fold_right (fun (h::t) uv -> Matrix.set false h uv) pathsN uv in
            aux uvn (otherPaths @ pathsN)
    in
    aux unVisited [[start]]
*)


let solve m start goal =
  let open AStar in
  let open AStar in
    let cost = distanceManhattan in
    let problemWithoutHallways = { cost; goal; get_next_states = get_next_states goal ~allowHallway:false ~isMapGen:true m; } in
    let problem = { cost; goal; get_next_states = get_next_states goal ~allowHallway:true ~isMapGen:true m; } in
    match search problemWithoutHallways start with
    | None -> search problem start |> Option.get
    | Some p -> p


let canMoveTo t = match t.t with
    (* TODO name not correct *)
    | Floor | StairsUp | StairsDown | Hallway HallRegular -> true
    | Door (Open, _) -> true
    | Door (Closed, _) | Door (Hidden, _) -> false
    | Hallway HallHidden -> false
    | Stone -> false
    | Unseen -> false
    | Wall Horizontal | Wall Vertical -> false

type fDir = pos -> pos

type actionsPlayer =
    | Drop of selectionItem list
    | MoveDir of fDir
    | Pickup of selectionItem list
    | Quaff of selectionItem
    | Read of selectionItem
    | Search
    | Throw of selectionItem * fDir
    | Wield of selectionItem
    | Zap of selectionItem * fDir

let creatureAddHp n t p (c : Creature.t) state =
    let cl = getCurrentMap state in
    let t' =
        if c.hp + n < 0 then
            (* TODO drops *)
            let _ = if playerCanSee state p then msgAdd state (sf "The %s is killed!" c.info.name) else () in
            let deathDrops =
                let corpse = Item.mkCorpse c.info.name c.info.color state.turns in
                (* TODO Not every creature can leave a corpse *)
                let item = if oneIn 6 then [Item.random ()] else [] in
                corpse::item @ c.inventory
            in
            { t with occupant = None; items = deathDrops @ t.items }
        else
            let c' = Creature { c with hp = c.hp + n } in
            { t with occupant = Some c' }
    in
    let cl' = Matrix.set t' p cl in
    setCurrentMap cl' state

type attackLanded =
    | Miss
    | MissBarely
    | Hit

let rollAttackLanded threshold addSides =
    let roll = rn 1 (20 + addSides) in
    match roll with
    | _ when roll < threshold -> Hit
    | _ when roll = threshold -> MissBarely
    | _ -> Miss

let reduceDamage ac damage =
    if ac >= 0 then damage else
    (rn ac (-1)) + damage |> min 1

let doCreaturePassive c state =
    let pAttacks = Cr.getAttacksPassive c in
    if L.is_empty pAttacks then state else

    L.fold_left
        ( fun state (pa : Hit.passive) ->
            let msgs = Hit.getMsgs (Hit.Passive pa) in
            msgAdd state (sf "It %s. The %s %s you." msgs.msgHit msgs.msgCause msgs.msgEffect);
            let damage = doRoll { rolls = c.info.levelBase + 1; sides = pa.maxRoll } in
            (* ^TODO based on current level *)
            playerAddHp (-damage) state
            (* ^TODO resistances *)
        )
        state
        pAttacks

let playerAttackMelee t p (c : Creature.t) state =
    (* TODO base on stats *)
    match rollAttackLanded 15 0 with
    | Miss ->
        msgAdd state (sf "You miss the %s." c.info.name);
        state
    | MissBarely ->
        msgAdd state (sf "You just miss the %s." c.info.name);
        state
    | Hit ->
        msgAdd state (sf "You hit the %s." c.info.name);
        (* TODO creature AC *)
        let state = doCreaturePassive c state in
        let sp = state.statePlayer in
        let damage = match sp.weaponWielded with
            | None -> rn 1 2 (* bare-handed *)
            | Some w -> doRoll w.damage
        in
        creatureAddHp (-damage) t p c state

let priceShop i = (Item.getPriceBase i) * 4 / 3

let rec playerMove mf state =
    let p = state.statePlayer.pos in
    let m = getCurrentMap state in
    let pn = mf p in
    if not (isInMap pn) then state else

    match Matrix.get m pn with
    | { t = Door (Closed, ori); _ } as tile ->
        (* TODO make chance based on stats *)
        if oneIn 3 then
            let _ = msgAdd state "The door resists!" in
            state
        else
            let _  = msgAdd state "You open the door." in
            let tn = Matrix.set { tile with t = Door (Open, ori) } pn m in
            setCurrentMap tn state

    | { occupant = Some (Creature c); _ } as t ->
        playerAttackMelee t pn c state

    | { occupant = Some Boulder; _ } as t ->
        let pbNew = mf pn in
        if not (isInMap pbNew) then (msgAdd state "The boulder won't budge!"; state) else
        let behindBoulder = Matrix.get m pbNew in
        if canMoveTo behindBoulder |> not then (msgAdd state "The boulder won't budge."; state) else
        ( match behindBoulder with
            | { occupant = Some Boulder; _ } -> msgAdd state "There's something blocking the boulder!"; state
            | { occupant = Some _; _ } -> msgAdd state "There's something alive behind the boulder!"; state
            | _ ->
                let behind' = { behindBoulder with occupant = Some Boulder } in
                let t' = { t with occupant = None } in
                let m' =
                    Matrix.set behind' pbNew m
                    |> Matrix.set t' pn
                in
                msgAdd state "With great effort you push the boulder.";
                playerMove mf (setCurrentMap m' state)
        )

    | tNew ->
        if not (canMoveTo tNew) then state else
        let tile = Matrix.get m p in
        let statePlayer = { state.statePlayer with pos = pn } in
        let state' = { state with statePlayer } in
        let m' =
            Matrix.set { tile with occupant = None } p m
            |> Matrix.set { tNew with occupant = Some Player } pn
        in
        ( if not (List.is_empty tNew.items) then
            if List.length tNew.items > itemsDisplayedMax then
                msgAdd state "You see here many items."
            else
                let _ = msgAdd state "You see here:" in
                List.iter
                    ( fun i ->
                        let price = if playerIsInShop state' then sf "(%i zorkmids)" (priceShop i) else "" in
                        msgAdd state (sf "%s %s" (Item.nameDisplay i) price)
                    )
                    tNew.items
        else
            ()
        );

        { (setCurrentMap m' state') with statePlayer }

let playerSearch state =
    (* TODO base search success on stats *)
    let currentLevel = getCurrentMap state in
    let hiddenTerrainAround =
        posAround state.statePlayer.pos
        |> List.filter (fun pa -> Matrix.get currentLevel pa |> isTerrainHidden)
    in
    let terrain' = List.fold_left
        ( fun m p ->
            if not (oneIn 3) then
                m
            else
                let current = Matrix.get m p in
                let tt' = match current.t with
                    | Door (Hidden, ori) -> msgAdd state "You find a hidden door!"; Door (Closed, ori)
                    | Hallway HallHidden -> msgAdd state "You find a hidden hallway!"; Hallway HallRegular
                    | _ -> assert false
                in
                Matrix.set
                    { current with t = tt' }
                    p m
        ) currentLevel hiddenTerrainAround
    in

    setCurrentMap terrain' state

let moveCreature a b state =
    let level = getCurrentMap state in
    let ct = Matrix.get level a in
    let tt = Matrix.get level b in
    if Option.is_some tt.occupant then
        state
    else
    let level' = Matrix.set { ct with occupant = None } a level in
    let level'' = Matrix.set { tt with occupant = ct.occupant } b level' in
    setCurrentMap level'' state

let getCreaturePath m start goal =
  let open AStar in
  let open AStar in
    let problem =
        { cost = distanceManhattan
        ; goal
        ; get_next_states = get_next_states goal ~manhattan:false ~allowHallway:true ~isMapGen:false m
        }
    in
    match search problem start with
    | None -> None
    | Some path when List.length path <= 1 -> Some []
    | Some path ->
        Some
        ( path
        |> List.rev
        |> List.tl
        )

let getHitThreshold ac attackerLevel =
    let ac' = if ac < 0 then rn ac (-1) else ac in
    10 + ac' + attackerLevel |> max 1

let creatureAttackMelee (c : Creature.t) p state =
    if p = state.statePlayer.pos then
        let hitThreshold = getHitThreshold 10 c.level in
        (* ^TODO player AC *)
        c.info.hits
        |> List.filter (function | Hit.Melee _ -> true | Hit.Weapon _ -> true | _ -> false)
        |> List.mapi (fun i v -> i, v)
        |> List.fold_left
            ( fun state' (addSides, h) ->
                match rollAttackLanded hitThreshold addSides with
                | Miss ->
                    let _ = msgAdd state (sf "The %s misses you." c.info.name) in
                    state'
                | MissBarely ->
                    let _ = msgAdd state (sf "The %s just misses you." c.info.name) in
                    state'
                | Hit ->
                    let damage =
                        ( match h with
                        | Hit.Melee hm -> doRoll hm.stats.roll
                        | Hit.Weapon h ->
                            ( match Item.getWeaponMostDamaging c.inventory with
                            | None -> doRoll h
                            | Some w -> (doRoll h) + (doRoll w.damage)
                            )
                        | _ -> assert false
                        )
                        |> reduceDamage 10
                        (* ^TODO player AC *)
                    in

                    let msgsHit = Hit.getMsgs h in
                    msgAdd state (sf "The %s %s you." c.info.name msgsHit.msgHit);

                    playerAddHp (-damage) state'
            )
            state

    else
        let _ = assert false in
        state

let throw item pFrom dir range msgThrower state =
    let m = getCurrentMap state in
    let isMiss () = oneIn 4 in

    let rec getPosStart pc posLanded = match posAdd dir pc with
        | _ when pc = posLanded -> None
        | _ when playerCanSee state pc -> Some (posDiff dir pc)
        | pn -> getPosStart pn posLanded
    in
    let rec getPosEnd pc posLanded = match posAdd dir pc with
        | _ when posLanded = pc -> pc
        | pn when not (playerCanSee state pn) -> pc
        | pn -> getPosEnd pn posLanded
    in
    let msgCreatureMissed (c : Creature.t) p =
        if playerCanSee state p then
            [ sf "The %s misses the %s." (Item.name item) c.info.name ]
                    (* TODO invisible creatures *)
            else
                []
    in
    let msgCreatureHit (c : Creature.t) p = if playerCanSee state p then
        [ sf "The %s hits the %s." (Item.name item) c.info.name ]
            (* TODO invisible creatures *)
        else
            []
    in

    let rollDamage () = match item with
        | Item.Weapon w -> doRoll w.damage
        | _ -> rn 1 2
    in

    let rec doThrow item dir pc msgs range state = match posAdd dir pc with
        | _ when range <= 0 -> state, pc, msgs
        | pn when not (isInMap pn) -> state, pc, msgs
        | pn ->
            ( match Matrix.get m pn with
            | { occupant = Some Boulder; _ } -> doThrow item dir pn msgs (range - 1) state
            | { occupant = Some (Creature c); _ } as t ->
                ( match isMiss () with
                | true ->
                    let msg = msgCreatureMissed c pn in
                    doThrow item dir pn (msg @ msgs) (range - 1) state
                | false ->
                    let msg = msgCreatureHit c pn in
                    let damage = rollDamage () in
                    creatureAddHp (-damage) t pn c state, pn, msg @ msgs
                )
            | { occupant = Some Player; _ } ->
                ( match isMiss () with
                | true ->
                    let msg = sf "The %s misses you." (Item.name item) in
                    doThrow item dir pn (msg::msgs) (range - 1) state
                | false ->
                    let msg = sf "The %s hits you." (Item.name item) in
                    let damage = rollDamage () in
                    (playerAddHp (-damage) state), pn, msg::msgs
                )
            | t ->
                if canMoveTo t then
                    doThrow item dir pn msgs (range - 1) state
                else
                    state, pc, msgs
            )
    in

    let state, posLanded, msgs = doThrow item dir pFrom [] range state in
    let posStart = getPosStart pFrom posLanded in

    ( match posStart with
        | None -> () (* At no point did player see the item *)
        | Some ps ->
            ( if playerCanSee state pFrom
                then
                    msgAdd state msgThrower
                else
                    msgAdd state (sf "You see a %s fly." (Item.name item))
            );

            let animation =
                { dir = posAdd dir { row = 0; col = 0 }
                ; posStart = ps
                ; posCurrent = ps
                ; posEnd = getPosEnd ps posLanded
                ; image = imageOfItem item
                }
            in
            animate state ~cumulative:false ~linger:false animation;

            L.rev msgs
            |> L.iter (fun m -> msgAdd state m)
    );

    let m = getCurrentMap state in
    let tLanded = Matrix.get m posLanded in
    let tLanded = { tLanded with items = item::tLanded.items } in
    let m' = Matrix.set tLanded posLanded m in

    setCurrentMap m' state

let rangeThrownCreature = 8

let creatureThrow (c : Creature.t) p item dir state =

    let msgThrower = sf "The %s throws a %s." c.info.name (Item.name item) in (* TODO a vs an *)
    let state = throw item p dir rangeThrownCreature msgThrower state in

    let m = getCurrentMap state in
    let tCreature = Matrix.get m p in
    let creature = Creature { c with inventory = listRemove item c.inventory } in
    let tCreature = { tCreature with occupant = Some creature } in

    let m' = Matrix.set tCreature p m in
    setCurrentMap m' state

let getReflectedDir p dir state =
    assert (isInMap p);
    if 0 = dir.row || 0 = dir.col then dirRevBoth dir else
    (* ^ easy case for + shaped rays *)

    let m = getCurrentMap state in

    let tAlongDirCol = posDiff { dir with row = 0 } p |> Matrix.get m in
    let tAlongDirRow = posDiff { dir with col = 0 } p |> Matrix.get m in

    let dirReflectRow = dirRevRow dir in
    let dirReflectCol = dirRevCol dir in

    match canMoveTo tAlongDirCol, canMoveTo tAlongDirRow with
    | false, false -> dirRevBoth dir (* room (internal) corner *)
    | true, false -> dirReflectCol (* vertical reflection *)
    | false, true -> dirReflectRow (* horizontal reflection *)
    | true, true -> rnItem [ dirReflectCol; dirReflectRow ] (* exposed corner *)

let castRay (effect : Hit.effect) from dir roll state =
    let reductionRangeOnHit = 3 in
    let damage =
        doRoll roll
    in
    let range = 20 + rn 1 8 in

    let msgs = Hit.getMsgsEffect effect in
    let rec doRay from pc dir state range =
        if range <= 0 then state else
        let pn = posAdd pc dir in
        if not (isInMap pn) then state else

        let state, shouldReflect, range =
            let m = getCurrentMap state in match Matrix.get m pn with
            | { occupant = Some Boulder; _ } as t ->
                ( match effect with
                | Sonic ->
                    let t = { t with occupant = None } in
                    let m = Matrix.set t pn m in
                    msgAdd state (sf "The %s crumbles the boulder." msgs.msgCause);
                    setCurrentMap m state, false, range - reductionRangeOnHit
                | _ ->
                    msgAdd state (sf "The %s whizzes past the boulder." msgs.msgCause);
                    state, false, range
                )
            | { occupant = Some Creature c; _ } as t ->
                msgAdd state (sf "The %s %s the %s." msgs.msgCause msgs.msgEffect c.info.name);
                creatureAddHp (-damage) t pn c state, false, range - reductionRangeOnHit
                (* ^TODO resistances *)
            | { occupant = Some Player; _ } ->
                msgAdd state (sf "The %s %s you!" msgs.msgCause msgs.msgEffect);
                playerAddHp (-damage) state, false, range - reductionRangeOnHit
                (* ^TODO resistances *)

            | { occupant = None; _ } as t ->
                if not (canMoveTo t) then
                    state, true, range (* TODO reflection *)
                else
                    state, false, range (* TODO burning items, etc. *)
        in

        if shouldReflect || range <= 1 then
            let animation =
                { dir
                ; posStart = from
                ; posCurrent = from
                ; posEnd = if shouldReflect then pc else pn
                ; image = Hit.getImageForAnimation effect dir
                }
            in
            ( match effect with
            | Sonic -> ()
            | _ -> animate ~linger:(range <= 1) state animation
            (* TODO neater way? *)
            );

            if shouldReflect then
                doRay pn pn (getReflectedDir pn dir state) state (range - 1)
            else
                state
        else
            doRay from pn dir state (range - 1)
    in
    doRay from from dir state range

let creatureAttackRanged (c : Creature.t) cp tp state =
    c.info.hits
    |> List.filter (function | Hit.Ranged _ -> true | Weapon _ -> true | _ -> false)
    |> List.fold_left
        ( fun state' h ->
            let pDiff = posDiff cp tp in
            let pDir = posDir pDiff in

            match h with
                | Hit.Weapon _ ->
                    ( match Creature.getWeaponForThrow c with
                    | None -> assert false
                    | Some w -> creatureThrow c cp (Item.Weapon w) pDir state'
                    )
                | Ranged hr as h ->
                    let hs = hr.stats in
                    let msgsHit = Hit.getMsgs h in
                    msgAdd state (sf "The %s %s %s." c.info.name msgsHit.msgHit msgsHit.msgCause);
                    castRay (Hit.getEffect h) cp pDir hs.roll state'
                | _ -> assert false
        )
        state

let creaturePickupWeapons (c : Creature.t) p state =
    let m = getCurrentMap state in
    let t = Matrix.get m p in
    let weps, remain = L.partition Item.isWeapon t.items in

    let c = { c with inventory = weps @ c.inventory } in
    let t = { t with items = remain; occupant = Some (Creature c) } in
    let m' = Matrix.set t p m in
    setCurrentMap m' state

let rec animateCreature c cp state =
    if not (Cr.hasTurn c) then state else
    let state = playerUpdateMapKnowledge state in
    let pp = state.statePlayer.pos in
    (* ^TODO allow attacking other creatures *)
    let m = getCurrentMap state in
    let canSeePlayer = creatureCanSee c cp pp state in

    let cpn, state' = if distance cp pp <= 1.5 then
        (* ^TODO blindness/confusion/etc. *)
            cp, creatureAttackMelee c pp state
        else if areLinedUp cp pp
                && Cr.hasAttackRanged c
                && canSeePlayer then
                (* ^TODO check that attack has path to target *)
            cp, creatureAttackRanged c cp pp state
        else
            let pathToPlayer = getCreaturePath m cp pp in
            let pathToWeapon =
                if not (Cr.hasAttackWeapon c.info) then
                    None
                else
                    match findItemMatchingInSight c Item.isWeapon cp state with
                    | [] -> None
                    | wp::_ -> getCreaturePath m cp wp
            in
            let moveAlongPath = function
                | None | Some [] -> cp, state
                | Some (np::_) -> np, moveCreature cp np state
            in
            match pathToWeapon with
                | None -> moveAlongPath pathToPlayer
                | Some [] when not canSeePlayer || oneIn 2 -> cp, creaturePickupWeapons c cp state
                | Some [] -> moveAlongPath pathToPlayer
                | Some _ when canSeePlayer -> moveAlongPath pathToPlayer
                | Some _ -> moveAlongPath pathToWeapon
    in
    let c = { c with pointsSpeed = c.pointsSpeed - pointsSpeedPerTurn } in
    animateCreature c cpn state'

let animateCreatures state = Matrix.foldI
    ( fun _ p state' -> function
        | { occupant = Some (Creature c); _ } ->
            let state = playerUpdateMapKnowledge state' in
            animateCreature c p state
        | _ -> state'
    ) state (getCurrentMap state)

let maybeAddCreature state =
    if oneIn 50 then
        placeCreature ~room:None state
    else
        state

let playerCheckHp state =
    let sp = state.statePlayer in
    if sp.hp <= 0 then
        let _ = msgAdd state "You died..." in
        None
    else
        Some state

let selectionOfItems ~single oc l =
    L.filter_map id l
    |> listTake 26
    |> L.mapi
        ( fun ix (iix, i) ->
            { letter = 0x61 (* 'a' *) + ix |> Char.chr
            ; iIndex = iix
            ; name = Item.nameDisplay i
            ; selected = false
            }
        )
    |>  ( fun l ->
            { sItems = l
            ; single
            ; onComplete = oc
            }
        )

let playerQuaff si state =
    let sp = state.statePlayer in
    let item = List.nth sp.inventory si.iIndex in
    match item with
        | Container _ -> msgAdd state "What a silly thing to quaff!"; state
        | Corpse _ -> msgAdd state "What a silly thing to quaff!"; state
        | Gold _ -> msgAdd state "You were unable to swallow the gold piece."; state
        | Scroll _ -> msgAdd state "This scroll is quite solid. Quite difficult to drink..."; state
        | Weapon _ -> msgAdd state "You change your mind about swallowing your weapon."; state
        | Wand _ -> msgAdd state "You change your mind about swallowing your wand."; state
        | Potion p ->
            let inventory, _ = partitionI (fun i _ -> i <> si.iIndex) sp.inventory in
            let statePlayer = { sp with inventory } in
            let state = { state with statePlayer } in
            match p.potion_t with
            | Healing -> msgAdd state "You feel better."; playerAddHp (8 + (doRoll {sides=4; rolls=4})) state
            | HealingExtra -> msgAdd state "You feel much better."; playerAddHp (16 + (doRoll {sides=4; rolls=8})) state
            | HealingFull -> msgAdd state "You feel completely healed."; playerAddHp(sp.hpMax) state
            | Sickness -> msgAdd state "This tastes like poison."; playerAddHp(rn (-100) (-10)) state

let playerRead si state =
    let sp = state.statePlayer in
    let item = List.nth sp.inventory si.iIndex in
    match item with
        | Container _ -> msgAdd state "What a silly thing to read!"; state
        | Corpse _ -> msgAdd state "What a silly thing to read!"; state
        | Gold _ -> msgAdd state "The gold is shiny!"; state
        | Potion _ -> msgAdd state "This potion is unlabeled"; state
        | Weapon _ -> msgAdd state "There's nothing to read on this weapon."; state
        | Wand _ -> msgAdd state "This is indeed a wand."; state
        | Scroll s ->
            let inventory, _ = partitionI (fun i _ -> i <> si.iIndex) sp.inventory in
            let statePlayer = { sp with inventory } in
            let state = { state with statePlayer } in
            match s.scroll_t with
            | CreateMonster -> msgAdd state "The area feels more dangerous!"; placeCreatures ~preferNearby:true ~room:None (rn 1 5) state
            | MagicMapping -> msgAdd state "An image coalesces in your mind."; setKnowledgeCurrentMap (getCurrentMap state) state (* TODO remove item positions *)
            | Teleport ->
                msgAdd state "Your position feels more uncertain.";
                let pp = sp.pos in
                let m = getCurrentMap state in
                let spawnPositions =
                    allMapPositions
                    |> L.filter (fun p -> canSpawnHere ~forbidPos:None m p)
                in
                let pNew = rnItem spawnPositions in
                let mf = posDiff pp pNew |> posAdd in
                playerMove mf state

let playerWield si state =
    let sp = state.statePlayer in
    let item = List.nth sp.inventory si.iIndex in
    let weaponWielded = Some (Item.toWeapon item) in
    msgAdd state (sf "You wield %s." (Item.nameDisplay item));

    let unwielded = match sp.weaponWielded with
        | None -> []
        | Some w -> [Item.(Weapon w)]
    in

    let inventory = L.filteri (fun i _ -> i <> si.iIndex) sp.inventory in
    let inventory = unwielded @ inventory in
    let statePlayer =
        { sp with weaponWielded
        ; inventory
        }

    in
    { state with statePlayer }

let playerZap si fDir state =
    let sp = state.statePlayer in
    let item = List.nth sp.inventory si.iIndex in
    let item = match item with
        | Item.Wand w -> Item.Wand { w with charges = max 0 (w.charges - 1) }
        | _ -> assert false
    in
    let inventory = listSet si.iIndex item sp.inventory in
    let statePlayer = { sp with inventory } in
    let state = { state with statePlayer } in

    let dir = fDir { row = 0; col = 0 } in

    match item with
        | Wand w ->
            if w.charges <= 0 then
                let _ = msgAdd state "nothing happens." in
                state
            else
            ( match w.wand_t with
            | Fire ->
                msgAdd state "A column of fire erupts from your wand.";
                castRay Hit.Fire sp.pos dir { rolls = 6; sides = 6 } state
            | MagicMissile ->
                msgAdd state "A hail of particles shoots from your wand.";
                castRay Hit.Physical sp.pos dir { rolls = 2; sides = 6 } state
            | Striking ->
                msgAdd state "Your wand emits a loud burst.";
                castRay Hit.Sonic sp.pos dir { rolls = 1; sides = 6 } state
                (* TODO it's invisible and crumbles boulders *)
            )
        | _ -> msgAdd state "can't zap that."; state

let playerDrop sl state =
    let sI = L.map (fun s -> s.iIndex) sl in
    let sp = state.statePlayer in
    let m = getCurrentMap state in
    let t = Matrix.get m sp.pos in

    let iDropped, iRemain = partitionI (fun ix _ -> contains sI ix) sp.inventory in

    (* TODO allow dropping gold *)
    let itemsValue = L.fold_left (fun acc i -> acc + (Item.getPriceBase i)) 0 iDropped in
    let itemsValueTrade = itemsValue / 2 in

    if playerIsInShop state && L.exists Item.isCorpse iDropped then
        let _ = msgAdd state "Keep that filthy corpse out of my shop!" in
        state
    else

    let gold =
        if playerIsInShop state then
            let _ = msgAdd state (sf "Thank you! Here's %i zorkmids for you." itemsValueTrade) in
            sp.gold + itemsValueTrade
        else
            sp.gold
    in

    let inventory = iRemain in
    let statePlayer = { sp with gold; inventory } in

    let m' = Matrix.set { t with items = iDropped @ t.items } sp.pos m in
    { (setCurrentMap m' state) with statePlayer }

let playerPickup sl state =
    let sI = L.map (fun s -> s.iIndex) sl in
    let sp = state.statePlayer in
    let m = getCurrentMap state in
    let t = Matrix.get m sp.pos in

    let iTaken, iRemain = partitionI (fun ix _ -> contains sI ix) t.items in

    let goldTaken, iTaken = L.partition_map (function | Item.Gold n -> Left n | i -> Right i) iTaken in
    let totalGoldTaken = List.fold_left (+) 0 goldTaken in

    if playerIsInShop state then
        match totalGoldTaken, iTaken with
        | goldTaken, _ when goldTaken > 0 -> msgAdd state "Hey! That's not your gold!"; state
        | _, iTaken ->
            let itemsValue = L.fold_left (fun t i -> t + (priceShop i)) 0 iTaken in
            if itemsValue > sp.gold then
                let _ = msgAdd state "You can't afford that!" in
                state
            else
                let gold = sp.gold - itemsValue in
                let statePlayer = { sp with inventory = iTaken @ sp.inventory; gold } in
                let m' = Matrix.set { t with items = iRemain } sp.pos m in
                { (setCurrentMap m' state) with statePlayer }

    else

    let gold = sp.gold + totalGoldTaken in
    let statePlayer = { sp with inventory = iTaken @ sp.inventory; gold } in
    let m' = Matrix.set { t with items = iRemain } sp.pos m in
    { (setCurrentMap m' state) with statePlayer }
    (* ^TODO combine items *)

let playerThrow si fDir state =
    let rangeThrown = 6 in (* TODO *)
    let sp = state.statePlayer in
    let dir = fDir { row = 0; col = 0 } in

    let item = List.nth sp.inventory si.iIndex in

    let msgThrower = sf "You throw the %s." (Item.name item) in (* TODO a vs an *)
    let state = throw item sp.pos dir rangeThrown msgThrower state in

    let inventory = L.filteri (fun i _ -> i <> si.iIndex) sp.inventory in
    let statePlayer = { sp with inventory } in

    { state with statePlayer }

let rotCorpses state =
    let m = getCurrentMap state in

    let m' = Matrix.foldI
        ( fun _ p m t -> match t.items with
            | [] -> m
            | items ->
                let items = Item.rotCorpses state.turns items in
                Matrix.set { t with items } p m
        ) m m
    in

    let sp = state.statePlayer in
    let inventory = Item.rotCorpses state.turns sp.inventory in

    let statePlayer = { sp with inventory } in
    let state = { state with statePlayer } in
    setCurrentMap m' state

let incTurns state =
    { state with turns = state.turns + 1 }

let actionPlayer a state =
    Q.clear state.messages;
    let s' = match a with
        | Drop sl -> playerDrop sl state
        | MoveDir mf -> playerMove mf state
        | Pickup sl -> playerPickup sl state
        | Quaff si -> playerQuaff si state
        | Read si -> playerRead si state
        | Search -> playerSearch state
        | Throw (si, dir) -> playerThrow si dir state
        | Wield si -> playerWield si state
        | Zap (si, dir) -> playerZap si dir state
    in
    rotCorpses s'
    |> playerUpdateMapKnowledge
    |> animateCreatures
    (* TODO update playerMap after each creature move *)
    |> maybeAddCreature
    |> incTurns
    |> playerKnowledgeDeleteCreatures
    |> playerUpdateMapKnowledge
    |> playerAddHp (if oneIn 3 then 1 else 0) (* TODO player hp can go to 0 then back up *)
    |> playerCheckHp

let handleSelectItems k s state = match k with
    | ' ' ->
        let selected = L.filter (fun s -> s.selected) s.sItems in
        let nSelected = L.length selected in
        if nSelected = 0 || s.single && nSelected > 1 then
            (* TODO give feedback to player *)
            Some state
        else
        let firstSelected = List.hd selected in
        let state = { state with mode = Playing } in
        ( match s.onComplete with
            | DoDrop -> actionPlayer (Drop selected) state
            | DoPickup -> actionPlayer (Pickup selected) state
            | DoQuaff -> actionPlayer (Quaff firstSelected) state
            | DoRead -> actionPlayer (Read firstSelected) state
            | DoWield -> actionPlayer (Wield firstSelected) state
            | SelectDirThrow ->
                    let mode = Selecting (SelectDir (DoThrow firstSelected)) in
                    Some { state with mode }
            | SelectDirZap ->
                    let mode = Selecting (SelectDir (DoZap firstSelected)) in
                    Some { state with mode }

        )
    | ',' ->
        let hasUnselected = L.exists (fun s -> not s.selected) s.sItems in
        let selected = hasUnselected in
        let sItems = List.map (fun si -> { si with selected }) s.sItems in
        let mode = Selecting (SelectItems { s with sItems }) in

        Some { state with mode }

    | k ->
        match L.find_index (fun s -> k = s.letter) s.sItems with
        | None -> Some state
        | Some i ->
            let si = L.nth s.sItems i in
            let sItems = if s.single then
                L.map (fun s -> { s with selected = false }) s.sItems
            else
                s.sItems
            in
            let sItems = listSet i { si with selected = not (si.selected) } sItems in
            let mode = Selecting (SelectItems { s with sItems }) in
            Some { state with mode }

let handleSelectDir k sd state =
    let dir = match k with
        | 'y' -> Some northWest
        | 'u' -> Some northEast
        | 'b' -> Some southWest
        | 'n' -> Some southEast
        | 'k' -> Some north
        | 'j' -> Some south
        | 'l' -> Some east
        | 'h' -> Some west
        | _ -> None
    in
    match dir with
        | None -> Some state
        | Some dir ->
            let mode = Playing in
            let state = { state with mode } in
            ( match sd with
            | DoThrow si -> actionPlayer (Throw (si, dir)) state
            | DoZap si -> actionPlayer (Zap (si, dir)) state
            (* TODO wand charge is still used up if direction is cancelled (Escape) *)
            )

let handleSelect k s state = match s with
    | SelectDir sd -> handleSelectDir k sd state
    | SelectItems si -> handleSelectItems k si state

let terrainAddRoom m room =
    let rp = getRoomPositions room in
    let withFloor = List.fold_left
        ( fun m p ->
            Matrix.set
                { t = Floor
                ; occupant = None
                ; items = []
                }
                p m
        ) m rp
    in
    let olR = getOutliningRoom room in
    let outline = getBorder olR in
    let withWalls = List.fold_left
        ( fun m p ->
            let alignment = match p with
                | _ when p.row = olR.posNW.row -> Horizontal
                | _ when p.row = olR.posSE.row -> Horizontal
                | _ -> Vertical
            in
            Matrix.set { t = Wall alignment; occupant = None; items = [] } p m
        )
        withFloor
        outline
    in
    List.fold_left
        ( fun m d ->
            let stateDoor = match rn 0 5 with
                | 0 -> Hidden
                | 1 | 2 | 3 -> Closed
                | 4 | 5 -> Open
                | _ -> assert false
            in
            let ori = match Matrix.get m d with
                | { t = Wall ori; _ } -> ori
                | _ -> assert false
            in
            Matrix.set
                { t = Door (stateDoor, ori)
                ; occupant = None
                ; items = []
                }
                d m
        ) withWalls room.doors

let terrainAddRooms rooms t =
    List.fold_right (fun r m -> terrainAddRoom m r) rooms t

let maybeAddBoulder hallway m =
    if not (oneIn 50) then m else
    let p = rnItem hallway in
    let t = Matrix.get m p in
    Matrix.set { t with occupant = Some Boulder; t = Hallway HallRegular } p m

let terrainAddHallways rooms m =
    let allDoors = List.map (fun r -> r.doors) rooms |> List.concat in

    let rec aux m = function
        | [] -> m
        | _::[] -> m
        | d1::d2::t ->
            let path = solve m d1 d2
                (* remove door positions *)
                |> List.tl
                |> List.rev
                |> List.tl
            in
            let m = List.fold_left
                ( fun m p ->
                    let tCurrent = Matrix.get m p in
                    if isHallway tCurrent then
                        m
                    else
                        let hT = if oneIn 100 then HallHidden else HallRegular in
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

type stairDirection = Up | Down

let rec terrainAddStairs ~dir rooms m =
    let stairType = match dir with
        | Up -> StairsUp
        | Down -> StairsDown
    in
    let stairs = { t = stairType; occupant = None; items = [] } in
    match rooms with
        | [] -> assert false
        | rooms ->
            let r = rnItem rooms in
            let p = randomRoomPos r in
            if Matrix.get m p |> isStairs then
                terrainAddStairs ~dir rooms m
            else
                Matrix.set stairs p m

let rec placeRoomCreatures rooms state =
    match rooms with
        | [] -> state
        | r::ro ->
            let s' = placeCreature ~room:(Some r) state in
            if oneIn 3 then
                placeRoomCreatures ro s'
            else
                s'

let playerMoveToStairs ~dir state =
    let stairType = match dir with
        | Up -> StairsUp
        | Down -> StairsDown
    in
    let m = getCurrentMap state in
    let posStairs = getPosTerrain m stairType in
    let t = Matrix.get m posStairs in
    let m' = Matrix.set { t with occupant = Some Player } posStairs m in
    let statePlayer = { state.statePlayer with pos = posStairs } in
    { (setCurrentMap m' state) with statePlayer;  }

let addItem ~gold state m p =
    let t = Matrix.get m p in
    let i =
        if gold then
            let d = getDepthNext state in
            Item.rnGold d
        else
            Item.random ()
    in

    let t' = { t with items = i::t.items } in
    Matrix.set t' p m

let maybeAddItem ~gold room state m =
    if not (oneIn 3) then m else
    let p = randomRoomPos room in
    addItem ~gold state m p

let terrainAddObjects rooms state m =
    List.fold_left
        ( fun m' r -> match r.room_t with
            | Regular ->
                m'
                |> maybeAddItem ~gold:false r state
                |> maybeAddItem ~gold:true r state

            | Shop shop ->
                getRoomPositions r
                |> L.filter (fun p -> p <> shop.posEntry)
                |> L.fold_left (fun m p -> addItem ~gold:false state m p) m'
        )
        m
        rooms

let mapGen state =
    let rooms = roomsGen () in
    let terrain = Matrix.fill mapSize { t = Stone; occupant = None; items = [] }
        |> terrainAddRooms rooms
        |> terrainAddStairs ~dir:Up rooms
        |> terrainAddStairs ~dir:Down rooms
        |> terrainAddHallways rooms
    in
    let rooms = maybeMakeShop rooms state terrain in
    let map = terrainAddObjects rooms state terrain in
    addLevel { rooms; map } state
    |> playerMoveToStairs ~dir:Up
    |> placeRoomCreatures rooms


let playerGoUp state =
    (* ^ TODO move to actionPlayer *)
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentMap state) p |> isTerrainOf StairsUp |> not then
        state
    else
        let sl = state.stateLevels in
        if sl.indexLevel = 0 then
            state
        else
            setIndexLevel (sl.indexLevel - 1) state
            |> rotCorpses
            |> playerMoveToStairs ~dir:Down
            |> playerUpdateMapKnowledge

let playerGoDown state =
    (* ^ TODO move to actionPlayer *)
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentMap state) p |> isTerrainOf StairsDown |> not then
        state
    else
        let sl = state.stateLevels in
        if sl.indexLevel = List.length sl.levels - 1 then
            mapGen state
            |> playerMoveToStairs ~dir:Up
            |> playerAddMapKnowledgeEmpty
            |> playerUpdateMapKnowledge
        else
            setIndexLevel (sl.indexLevel + 1) state
            |> rotCorpses
            |> playerMoveToStairs ~dir:Up
            |> playerUpdateMapKnowledge

let modeSelectDrop state =
    let inv = state.statePlayer.inventory in
    let items = L.mapi (fun ix i -> Some (ix, i)) inv in
    let selection = selectionOfItems ~single:false DoDrop items in
    match items with
        | [] ->
            let _ = msgAdd state "You don't have anything you can drop." in
            Some state
        | _ ->
            let mode = Selecting (SelectItems selection) in
            Some { state with mode }

let modeSelectPickup state =
    let pp = state.statePlayer.pos in
    let m = getCurrentMap state in
    let t = Matrix.get m pp in
    let items = L.mapi (fun ix i -> Some (ix, i)) t.items in
    let selection = selectionOfItems ~single:false DoPickup items in
    match t.items with
        | [] ->
            let _ = msgAdd state "There's nothing to pick up here." in
            Some state
        | _::[] ->
            actionPlayer (Pickup selection.sItems) state
        | _ ->
            let mode = Selecting (SelectItems selection) in
            Some { state with mode }

let modeSelectRead state =
    let sp = state.statePlayer in
    let readables = L.mapi (fun ix i -> if Item.isReadable i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some readables) then Some state else
    let selection = selectionOfItems ~single:true DoRead readables in
    let mode = Selecting (SelectItems selection) in
    Some { state with mode }

let modeSelectQuaff state =
    let sp = state.statePlayer in
    let quaffables = L.mapi (fun ix i -> if Item.isQuaffable i then Some (ix, i) else None) sp.inventory in
    (* ^TODO refactor *)
    if not (L.exists Option.is_some quaffables) then Some state else
    let selection = selectionOfItems ~single:true DoQuaff quaffables in
    let mode = Selecting (SelectItems selection) in
    Some { state with mode }

let modeSelectThrow state =
    (* ^TODO refactor *)
    let sp = state.statePlayer in
    let throwables = L.mapi (fun ix i -> if Item.isWeapon i then Some (ix, i) else None) sp.inventory in
    (* TODO allow throwing non-weapons *)
    if not (L.exists Option.is_some throwables) then Some state else
    let selection = selectionOfItems ~single:true SelectDirThrow throwables in
    let mode = Selecting (SelectItems selection) in
    Some { state with mode }

let modeSelectWield state =
    let sp = state.statePlayer in
    let wieldables = L.mapi (fun ix i -> if Item.isWeapon i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some wieldables) then Some state else
    let selection = selectionOfItems ~single:true DoWield wieldables in
    let mode = Selecting (SelectItems selection) in
    Some { state with mode }

let modeSelectZap state =
    (* ^TODO refactor *)
    let sp = state.statePlayer in
    let zappables = L.mapi (fun ix i -> if Item.isZappable i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some zappables ) then Some state else
    let selection = selectionOfItems ~single:true SelectDirZap zappables in
    let mode = Selecting (SelectItems selection) in
    Some { state with mode }

let modePlaying event state = match event with
    | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) -> actionPlayer (MoveDir west) state
    | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) -> actionPlayer (MoveDir east) state
    | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) -> actionPlayer (MoveDir north) state
    | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) -> actionPlayer (MoveDir south) state

    | `Key (`ASCII 'y', _) -> actionPlayer (MoveDir northWest) state
    | `Key (`ASCII 'u', _) -> actionPlayer (MoveDir northEast) state
    | `Key (`ASCII 'b', _) -> actionPlayer (MoveDir southWest) state
    | `Key (`ASCII 'n', _) -> actionPlayer (MoveDir southEast) state

    | `Key (`ASCII 's', _) -> actionPlayer Search state

    | `Key (`ASCII 'd', _) -> modeSelectDrop state
    | `Key (`ASCII ',', _) -> modeSelectPickup state
    | `Key (`ASCII 'q', _) -> modeSelectQuaff state
    | `Key (`ASCII 'r', _) -> modeSelectRead state
    | `Key (`ASCII 't', _) -> modeSelectThrow state
    | `Key (`ASCII 'w', _) -> modeSelectWield state
    | `Key (`ASCII 'z', _) -> modeSelectZap state

    | `Key (`ASCII '<', _) -> Some (playerGoUp state)
    | `Key (`ASCII '>', _) -> Some (playerGoDown state)
    | _ -> Some state

let modeSelecting event state s = match event with
    | `Key (`Escape, _) -> Some { state with mode = Playing }
    | `Key (`ASCII k, _)  -> handleSelect k s state
    | _ -> Some state

let update event state = match state.mode with
    | Playing -> modePlaying event state
    | Selecting s -> modeSelecting event state s

let stateInitial =
    Random.init 613;

    let stateLevels =
        { indexLevel = -1
        ; levels = []
        }
    in

    let statePlayer =
        { pos = { row = 0; col = 0 }
        ; gold = 20
        ; hp = 66
        ; hpMax = 66
        ; weaponWielded = None
        ; inventory = []
        ; knowledgeLevels = []
        }
    in

    let stateI =
        { stateLevels
        ; statePlayer
        ; messages = Q.create ()
        ; mode = Playing
        ; turns = 0
        }
    in
    Q.push "Welcome." stateI.messages;
    Q.push "Lucky! There's a full moon tonight." stateI.messages;
    mapGen stateI
    |> playerMoveToStairs ~dir:Up
    |> playerAddMapKnowledgeEmpty
    |> playerUpdateMapKnowledge

let () =
    let rec go state =
        imageCreate state;

        match Term.event term with
        | `End | `Key (`ASCII 'C', [`Ctrl]) -> ()
        | `Resize _ -> go state
        | #Unescape.event as e -> match update e state with
            | Some s -> go s
            | None -> ()
    in
    go stateInitial
