open Notty

open Common
open Matrix

module F = Format
module L = List
module Q = Queue

module Term = Notty_unix.Term

let term = Term.create ()

let brown = A.(fg yellow)

let sf = F.sprintf

let mapSize =
    { row = 21
    ; col = 80
    }

let distanceSight = 3.17

let itemsDisplayedMax = 5

let id a = a

let range min max = List.init (max - min + 1) (fun i -> i + min)

let contains l v = List.find_opt ((=) v) l |> Option.is_some

let partitionI f l =
    L.mapi (fun i v -> i, v) l
    |> L.partition_map (fun (i, v) -> if f i v then Left v else Right v)

let listMin l =
    let first = List.hd l in
    List.fold_left (fun minSofar v -> min minSofar v) first l

let listTake n = L.filteri (fun i _ -> i < n)

type wh =
    { w : int
    ; h : int
    }

type room =
    { posNW : pos
    ; posSE : pos
    ; doors : pos list
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

type flagItem =
    | Readable

type itemStats =
    { count : int
    ; iFlags : flagItem list
    }

type scroll_t =
    | CreateMonster
    | MagicMapping

type scroll =
    { itemStats : itemStats
    ; scroll_t : scroll_t
    }

type container_t =
    | Sack
    | Chest

type container =
    { container_t : container_t
    ; items : item list
    }

and item =
    | Gold of int
    | Scroll of scroll
    | Container of container

type roll =
    { rolls : int
    ; sides : int
    }

type hitEffect =
    | Physical
    | Fire

type passive =
    { maxRoll : int
    ; hitEffect : hitEffect
    }

type melee =
    | Bite
    | Claw

type ranged =
    | Breath

type hitStats =
    { roll : roll
    ; effect : hitEffect
    }

type hitRanged =
    { ranged_t : ranged
    ; hitStatsR : hitStats
    }

type hitMelee =
    { melee_t : melee
    ; hitStats : hitStats
    }

type hit =
    | Passive of passive
    | Ranged of hitRanged
    | Melee of hitMelee

type creatureInfo =
    { symbol : string
    ; color : A.color
    ; name : string
    ; difficulty : int
    ; levelBase : int
    ; hits: hit list
    }

type creature =
    { cHp : int
    ; level : int
    ; creatureInfo : creatureInfo
    }

type occupant = Creature of creature | Player | Boulder

type tile =
    { t : terrain
    ; occupant : occupant option
    ; items : item list
    }

type levels = tile Matrix.t list
type stateLevels =
    { indexLevel : int
    ; levels : levels
    }

type statePlayer =
    { pos : pos
    ; gold : int
    ; hp : int
    ; hpMax : int
    ; inventory : item list
    ; knowledgeLevels : levels
    }

type selectionItem =
    { iIndex : int
    ; name : string
    ; selected : bool
    ; letter : char
    }

type onSelectComplete = DoPickup | DoRead

type selection =
    { sItems : selectionItem list
    ; single : bool
    ; complete : bool
    ; onComplete : onSelectComplete
    }

type mode =
    | Playing
    | Selecting of selection

type state =
    { stateLevels : stateLevels
    ; statePlayer : statePlayer
    ; messages : string Q.t
    ; mode : mode
    (* objects : list Object *)
    }

type animation =
    { dir : pos
    ; posStart : pos
    ; posCurrent : pos
    ; posEnd : pos
    ; image : image
    }

type msgHit =
    { msgHit : string
    ; msgCause : string
    ; msgEffect : string
    }

let getEffect = function
    | Passive p -> assert false (* TODO *)
    | Ranged r -> r.hitStatsR.effect
    | Melee m -> m.hitStats.effect

let getMsgsCauseEffect a =
    let msgCause, msgEffect = match getEffect a with
        | Physical -> "attack", "hits"
        | Fire -> "fire", "burns"
    in
    { msgHit = ""; msgCause; msgEffect }

let getMsgsHit a =
    let msgHit = match a with
        | Passive p -> assert false (* TODO *)
        | Ranged r ->
            ( match r.ranged_t with
                | Breath -> "breathes"
            )

        | Melee m -> match m.melee_t with
            | Bite -> "bites"
            | Claw -> "claws at"
    in
    let msgBase = getMsgsCauseEffect a in
    { msgBase with msgHit }

let itemCount = function
    | Gold t -> t
    | Scroll { itemStats = is; _ } -> is.count
    | Container _ -> 1

let itemName i =
    let count = itemCount i in
    let mPlural = if count = 1 then "" else "s" in
    let name = match i with
        | Gold t -> "gold piece" ^ mPlural
        | Scroll { scroll_t = t; itemStats = is } -> "scroll" ^ mPlural ^ " of " ^
                ( match t with
                    | CreateMonster -> "create monster"
                    | MagicMapping -> "magic mapping"
                )
        | Container c -> match c.container_t with
            | Sack -> "sack"
            | Chest -> "chest"
    in
    sf "%i %s" count name

let itemHasFlag flag = function
    | Container _ -> false
    | Gold _ -> false
    | Scroll s ->  contains s.itemStats.iFlags flag

let mkHitMelee t e rolls sides = Melee
    { melee_t = t
    ; hitStats =
        { effect = e
        ; roll = { rolls; sides }
        }
    }

let mkHitRanged t e rolls sides = Ranged
    { ranged_t = t
    ; hitStatsR =
        { effect = e
        ; roll = { rolls; sides }
        }
    }

let addMsg state s = Q.push s state.messages

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

let posAdd a b =
    { row = a.row + b.row
    ; col = a.col + b.col
    }

let posDiff a b =
    { row = b.row - a.row
    ; col = b.col - a.col
    }

let posDir p =
    (* normalizes delta. WARN: only works lines on x or + shapes *)
    assert (p.row <> 0 || p.col <> 0);
    let den = max (abs p.row) (abs p.col) in
    { row = p.row / den
    ; col = p.col / den
    }

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

let isFloorOrStairsOpt = function
    | None -> false
    | Some t -> isFloorOrStairs t

let imageOfTile m pos = function
    | { occupant = Some occ; _ } ->
        ( match occ with
            | Creature c ->
                I.string A.(st bold ++ fg c.creatureInfo.color) c.creatureInfo.symbol
            | Player -> I.string A.(st bold ++ fg lightwhite) "@"
            | Boulder -> I.string A.(st bold ++ fg white) "0"
        )
    | { items = topItem::others; _ } ->
        let styles = if others = [] then A.(st bold) else A.(bg lightblack ++ st bold) in
        ( match topItem with
            | Gold _ -> I.string A.(styles ++ fg lightyellow) "$"
            | Scroll _ -> I.string A.(styles ++ fg white) "?"
            | Container _ -> I.string A.(styles ++ brown) "("
        )
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
                    getCurrentLevelKnowledge state
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
            s.sItems
            |> L.sort (fun s s'-> Char.compare s.letter s'.letter)
            |> L.map (fun s -> I.string A.empty (sf "%c %s %s" s.letter (if s.selected then "+" else "-") s.name))
            |> I.vcat
    )
    |> Term.image term

let rec animate state ?(animationLayer=[]) a =
    let animationLayer' =
        if a.posCurrent = a.posStart then
            []
        else
            (a.posCurrent, a.image)::animationLayer
    in
    imageCreate ~animationLayer:animationLayer' state;
    Unix.sleepf 0.05;

    if a.posCurrent = a.posEnd then
        Unix.sleepf 0.3
    else
    let posCurrent = posAdd a.posCurrent a.dir in
    let a' = { a with posCurrent } in

    animate state ~animationLayer:animationLayer' a'

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
    let m = getCurrentLevel state in
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
    let knowledgeEmpty = Matrix.fill mapSize unseenEmpty in
    let knowledgeLevels = state.statePlayer.knowledgeLevels @ [knowledgeEmpty] in
    let statePlayer = { state.statePlayer with knowledgeLevels } in
    { state with statePlayer }

let playerUpdateMapKnowledge state =
    let m  = getCurrentLevel state in
    let pk = getCurrentLevelKnowledge state in
    let pkUpdated = Matrix.foldI
        ( fun _ p pk' known ->
            let actual = Matrix.get m p in
            if actual <> known && playerCanSee state p then
                Matrix.set actual p pk'
            else
                pk'
        ) pk pk
    in
    setCurrentLevelKnowledge pkUpdated state

let playerKnowledgeDeleteCreatures state =
    let pk = getCurrentLevelKnowledge state in
    let pk' = Matrix.map
        ( fun v -> match v with
            | { occupant = Some (Creature _); _ } ->
                { v with occupant = None }
            | _ -> v
        ) pk
    in
    setCurrentLevelKnowledge pk' state


let rn min max = Random.int_in_range ~min ~max

let oneIn n = rn 0 (n - 1) = 0

let rnItem l =
    assert (List.length l > 0);

    let iLast = List.length l - 1 in
    List.nth l (rn 0 iLast)

let isInRoom room pos =
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

let allMapPositions =
    { posNW = {row = 0; col = 0}
    ; posSE = northWest mapSize
    ; doors = []
    }
    |> getRoomPositions


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

let placeCreature ?(preferNearby=false) ~room state =
    let m = getCurrentLevel state in
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
    let closestFirst = L.sort (fun p1 p2 -> Int.compare (distanceManhattan pp p1) (distanceManhattan pp p2)) in
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
            let creature =
                { cHp = 7 (* TODO pick creatures *)
                ; level = 15
                ; creatureInfo =
                    { symbol = "D"
                    ; color = A.lightred
                    ; name = "red dragon"
                    ; difficulty = 20
                    ; levelBase = 15
                    ; hits =
                        [ mkHitRanged Breath Fire 6 6
                        ; mkHitMelee Bite Physical 3 8
                        ; mkHitMelee Claw Physical 1 4
                        ; mkHitMelee Claw Physical 1 4
                        ]
                    }
                }
            in
            let map = getCurrentLevel state in
            let t = Matrix.get map p in
            let t' = { t with occupant = Some (Creature creature) } in
            let map' = Matrix.set t' p map in
            setCurrentLevel map' state

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
    | Floor | StairsUp | StairsDown | Hallway HallRegular -> true
    | Door (Open, _) -> true
    | Door (Closed, _) | Door (Hidden, _) -> false
    | Hallway HallHidden -> false
    | Stone -> false
    | Unseen -> false
    | Wall Horizontal | Wall Vertical -> false


type actionsPlayer =
    | MoveDir of (pos -> pos)
    | Pickup of selectionItem list
    | Read of selectionItem
    | Search

let creatureAddHp n t p c state =
    let cl = getCurrentLevel state in
    let t' =
        if c.cHp + n < 0 then
            (* TODO drops *)
            let _ = addMsg state (sf "The %s is killed!" c.creatureInfo.name) in
            { t with occupant = None }
        else
            let c' = Creature { c with cHp = c.cHp + n } in
            { t with occupant = Some c' }
    in
    let cl' = Matrix.set t' p cl in
    setCurrentLevel cl' state


let playerAttackMelee t p c state =
    (* TODO base on stats *)
    addMsg state (sf "You attack the %s." c.creatureInfo.name);
    creatureAddHp (-5) t p c state

let rec playerMove mf state =
    let p = state.statePlayer.pos in
    let m = getCurrentLevel state in
    let pn = mf p in
    if not (isInMap pn) then state else

    match Matrix.get m pn with
    | { t = Door (Closed, ori) } as tile ->
        (* TODO make chance based on stats *)
        if oneIn 3 then
            let _ = addMsg state "The door resists!" in
            state
        else
            let _  = addMsg state "You open the door." in
            let tn = Matrix.set { tile with t = Door (Open, ori) } pn m in
            setCurrentLevel tn state

    | { occupant = Some (Creature c); _ } as t ->
        playerAttackMelee t pn c state

    | { occupant = Some Boulder; _ } as t ->
        let pbNew = mf pn in
        if not (isInMap pbNew) then (addMsg state "The boulder won't budge!"; state) else
        let behindBoulder = Matrix.get m pbNew in
        if canMoveTo behindBoulder |> not then (addMsg state "The boulder won't budge."; state) else
        ( match behindBoulder with
            | { occupant = Some Boulder } -> addMsg state "There's something blocking the boulder!"; state
            | { occupant = Some _ } -> addMsg state "There's something alive behind the boulder!"; state
            | _ ->
                let behind' = { behindBoulder with occupant = Some Boulder } in
                let t' = { t with occupant = None } in
                let m' =
                    Matrix.set behind' pbNew m
                    |> Matrix.set t' pn
                in
                addMsg state "With great effort you push the boulder.";
                playerMove mf (setCurrentLevel m' state)
        )

    | tNew ->
        if not (canMoveTo tNew) then state else
        let tile = Matrix.get m p in
        let m' =
            Matrix.set { tile with occupant = None } p m
            |> Matrix.set { tNew with occupant = Some Player } pn
        in
        ( if not (List.is_empty tNew.items) then
            if List.length tNew.items > itemsDisplayedMax then
                addMsg state "You see here many items."
            else
                let _ = addMsg state "You see here:" in
                List.iter
                    ( fun i ->
                        addMsg state (itemName i)
                    )
                    tNew.items
        else
            ()
        );

        let pn = { state.statePlayer with pos = pn } in
        { (setCurrentLevel m' state) with statePlayer = pn }

let playerSearch state =
    (* TODO base search success on stats *)
    let currentLevel = getCurrentLevel state in
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
                    | Door (Hidden, ori) -> addMsg state "You find a hidden door!"; Door (Closed, ori)
                    | Hallway HallHidden -> addMsg state "You find a hidden hallway!"; Hallway HallRegular
                    | _ -> assert false
                in
                Matrix.set
                    { current with t = tt' }
                    p m
        ) currentLevel hiddenTerrainAround
    in

    setCurrentLevel terrain' state

let moveCreature a b state =
    let level = getCurrentLevel state in
    let ct = Matrix.get level a in
    let tt = Matrix.get level b in
    if Option.is_some tt.occupant then
        state
    else
    let level' = Matrix.set { ct with occupant = None } a level in
    let level'' = Matrix.set { tt with occupant = ct.occupant } b level' in
    setCurrentLevel level'' state

let getCreaturePath m start goal =
  let open AStar in
  let open AStar in
    let problem =
        { cost = distanceManhattan
        ; goal
        ; get_next_states = get_next_states goal ~manhattan:false ~allowHallway:true ~isMapGen:false m
        }
    in
    search problem start

let rollEffectSize roll =
    range 1 roll.rolls
    |> List.map (fun _ -> rn 1 roll.sides)
    |> List.fold_left (+) 0

let getHitThreshold ac attackerLevel =
    let ac' = if ac < 0 then rn ac (-1) else ac in
    10 + ac' + attackerLevel |> max 1

type miss =
    { missed : bool
    ; justMissed : bool
    }

let rollMiss threshold addSides =
    let roll = rn 1 (20 + addSides) in
    let justMissed = roll = threshold in
    { missed = roll >= threshold
    ; justMissed
    }

let rollReducedDamage ac damage =
    if ac >= 0 then damage else
    (rn ac (-1)) + damage |> min 1

let creatureAttackMelee c p state =
    if p = state.statePlayer.pos then
        let hitThreshold = getHitThreshold (-10) c.level in
        (* ^TODO player AC *)
        c.creatureInfo.hits
        |> List.filter_map (function | Melee hm -> Some hm | _ -> None)
        |> List.mapi (fun i v -> i, v)
        |> List.fold_left
            ( fun state' (addSides, hm) ->
                let miss = rollMiss hitThreshold addSides in
                if miss.missed then
                    let mJust = if miss.justMissed then " just" else "" in
                    let _ = addMsg state (sf "The %s%s misses you." c.creatureInfo.name mJust) in
                    state'
                else
                let effectSize =
                    rollEffectSize hm.hitStats.roll
                    |> rollReducedDamage 0
                    (* ^TODO player AC *)
                in
                let msgsHit = getMsgsHit (Melee hm) in

                addMsg state (sf "The %s %s you." c.creatureInfo.name msgsHit.msgHit);
                playerAddHp (-effectSize) state'
            )
            state

    else
        let _ = assert false in
        state

let getImageForAnimation t dir =
    let color = match t with
    | Physical -> A.(fg white)
    | Fire -> A.(fg lightred)
    in

    let c = match dir with
    | _ when dir.row = 0 -> "-"
    | _ when dir.col = 0 -> "|"
    | _ when dir.row = dir.col -> "\\"
    | _ when dir.row <> dir.col -> "/"
    | _ -> assert false
    in

    I.string A.(st bold ++ color) c

let creatureAttackRanged c cp tp state =
    let rec processPath effectSize msgsHit cp pd pTarget state =
        let cp' = posAdd cp pd in
        let m = getCurrentLevel state in

        let state' = match Matrix.get m cp' with
            | { occupant = Some Boulder; _ } -> addMsg state (sf "The %s whizzes past the boulder." msgsHit.msgCause); state (* TODO rays/vs weapons *)
            | { occupant = Some Creature c'; _ } as t ->
                    addMsg state (sf "The %s %s the %s." msgsHit.msgCause msgsHit.msgEffect c'.creatureInfo.name);
                    creatureAddHp (-effectSize) t cp' c' state
                    (* ^TODO resistances *)
            | { occupant = Some Player; _ } ->
                addMsg state (sf "The %s %s you!" msgsHit.msgCause msgsHit.msgEffect);
                playerAddHp (-effectSize) state

            | { occupant = None; _ } -> state (* TODO burning items, etc. *)
        in

        (* TODO stop ray when ranged sufficiently reduced
        -- not necessarily on target hit
        -- TODO Reflections
        *)
        if cp' = pTarget then
            state'
        else
            processPath effectSize msgsHit cp' pd pTarget state'
    in
    c.creatureInfo.hits
    |> List.filter_map (function | Ranged hr -> Some hr | _ -> None)
    |> List.fold_left
        ( fun state' hr ->
            let pDiff = posDiff cp tp in
            let pDir = posDir pDiff in
            let hs = hr.hitStatsR in
            let effectSize =
                rollEffectSize hs.roll
            in
            let animation =
                { dir = pDir
                ; posStart = cp
                ; posCurrent = cp
                ; posEnd = tp
                ; image = getImageForAnimation hs.effect pDir
                }
            in
            let msgsHit = getMsgsHit (Ranged hr) in
            addMsg state (sf "The %s %s %s." c.creatureInfo.name msgsHit.msgHit msgsHit.msgCause);
            animate state' animation;
            processPath effectSize msgsHit cp pDir tp state'
        )
        state

let hasRangedAttack c =
    c.creatureInfo.hits
    |> List.exists (function | Ranged _ -> true | _ -> false)

let animateCreature c cp state =
    let pp = state.statePlayer.pos in
    (* ^TODO allow attacking other creatures *)
    let cl = getCurrentLevel state in
    if distance cp pp <= 1.5 then
        (* ^TODO blindness/confusion/etc. *)
        creatureAttackMelee c pp state
    else if areLinedUp cp pp
            && hasRangedAttack c
            && creatureCanSee c cp pp state then
            (* ^TODO check that attack has path to target *)
        creatureAttackRanged c cp pp state
    else
        match getCreaturePath cl cp pp with
        | None -> state
        | Some path when List.length path <= 2 -> state
        | Some path ->
            (* Remove start and end points. TODO improve in aStar *)
            let h = path
                |> List.tl
                |> List.rev
                |> List.tl
                |> List.hd
            in
            moveCreature cp h state

let animateCreatures state = Matrix.foldI
    ( fun _ p state' -> function
        | { occupant = Some (Creature c); _ } ->
            let state = playerUpdateMapKnowledge state' in
            animateCreature c p state
        | _ -> state'
    ) state (getCurrentLevel state)

let maybeAddCreature state =
    if oneIn 50 then
        placeCreature ~room:None state
    else
        state

let playerCheckHp state =
    let sp = state.statePlayer in
    if sp.hp <= 0 then
        let _ = addMsg state "You died..." in
        None
    else
        Some state

let selectionOfItems ~single oc l =
    listTake 26 l
    |> L.mapi
        ( fun ix i ->
            { letter = 0x61 (* 'a' *) + ix |> Char.chr
            ; iIndex = ix
            ; name = itemName i
            ; selected = false
            }
        )
    |>  ( fun l ->
            { sItems = l
            ; single
            ; complete = false
            ; onComplete = oc
            }
        )

let playerRead si state =
    let sp = state.statePlayer in
    let item = List.nth sp.inventory si.iIndex in
    match item with
        | Container _ -> addMsg state "What a silly thing to read!"; state
        | Gold _ -> addMsg state "The gold is shiny!"; state
        | Scroll s ->
            let inventory, _ = partitionI (fun i _ -> i <> si.iIndex) sp.inventory in
            let statePlayer = { sp with inventory } in
            let state = { state with statePlayer } in
            match s.scroll_t with
            | CreateMonster -> addMsg state "The area feels more dangerous!"; placeCreatures ~preferNearby:true ~room:None (rn 1 5) state
            | MagicMapping -> addMsg state "An image coalesces in your mind."; setCurrentLevelKnowledge (getCurrentLevel state) state (* TODO remove item positions *)

let playerPickup sl state =
    let sI = L.map (fun s -> s.iIndex) sl in
    let sp = state.statePlayer in
    let m = getCurrentLevel state in
    let t = Matrix.get m sp.pos in

    let iTaken, iRemain = partitionI (fun ix _ -> contains sI ix) t.items in

    let goldTaken, iTaken = L.partition_map (function | Gold n -> Left n | i -> Right i) iTaken in
    let gold = sp.gold + (List.fold_left (+) 0 goldTaken) in

    let statePlayer = { sp with inventory = iTaken @ sp.inventory; gold } in
    let m' = Matrix.set { t with items = iRemain } sp.pos m in
    { (setCurrentLevel m' state) with statePlayer }
    (* ^TODO combine items *)

let actionPlayer a state =
    Q.clear state.messages;
    let s' = match a with
        | MoveDir mf -> playerMove mf state
        | Pickup sl -> playerPickup sl state
        | Read si -> playerRead si state
        | Search -> playerSearch state
    in
    playerUpdateMapKnowledge s'
    |> animateCreatures
    (* TODO update playerMap after each creature move *)
    |> maybeAddCreature
    |> playerKnowledgeDeleteCreatures
    |> playerUpdateMapKnowledge
    |> playerAddHp (if oneIn 3 then 1 else 0)
    |> playerCheckHp

let handleSelect k s state = match k with
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
            | DoPickup -> actionPlayer (Pickup selected) state
            | DoRead -> actionPlayer (Read firstSelected) state
        )
    | ',' ->
        let hasUnselected = L.exists (fun s -> not s.selected) s.sItems in
        let selected = hasUnselected in
        let sItems = List.map (fun si -> { si with selected }) s.sItems in
        let mode = Selecting { s with sItems } in

        Some { state with mode }

    | k ->
        match L.find_index (fun s -> k = s.letter) s.sItems with
        | None -> Some state
        | Some i ->
            let si = L.nth s.sItems i in
            let sItems = listSet i { si with selected = not (si.selected) } s.sItems in
            let mode = Selecting { s with sItems } in
            Some { state with mode }

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
                | { t = Wall ori } -> ori
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
                        let hT = if oneIn 50 then HallHidden else HallRegular in
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
    let m = getCurrentLevel state in
    let posStairs = getPosTerrain m stairType in
    let t = Matrix.get m posStairs in
    let m' = Matrix.set { t with occupant = Some Player } posStairs m in
    let statePlayer = { state.statePlayer with pos = posStairs } in
    { (setCurrentLevel m' state) with statePlayer;  }

let terrainAddObjects rooms m =
    List.fold_left
        ( fun m' r ->
            if not (oneIn 3) then m' else
            let p = randomRoomPos r in
            let t = Matrix.get m p in
            let count = 1 in
            let scroll = Scroll
                { itemStats = {count; iFlags = [Readable]}
                ; scroll_t =
                    if oneIn 2 then
                        CreateMonster
                    else
                        MagicMapping
                }
            in
            let i = match rn 0 2 with
                | 0 -> Gold (rn 13 6317)
                | 1 -> scroll
                | 2 -> Container
                    { container_t = Chest
                    ; items = [Gold (rn 313 6317); scroll]
                    }
                | _ -> assert false
            in

            let t' = { t with items = i::t.items } in
            Matrix.set t' p m'
        )
        m
        rooms

let mapGen state =
    let rooms = roomsGen () in
    let terrain = Matrix.fill mapSize { t = Stone; occupant = None; items = [] }
        |> terrainAddRooms rooms
        |> terrainAddObjects rooms
        |> terrainAddStairs ~dir:Up rooms
        |> terrainAddStairs ~dir:Down rooms
        |> terrainAddHallways rooms
    in
    addLevel terrain state
    |> playerMoveToStairs ~dir:Up
    |> placeRoomCreatures rooms


let playerGoUp state =
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentLevel state) p |> isTerrainOf StairsUp |> not then
        state
    else
        let sl = state.stateLevels in
        if sl.indexLevel = 0 then
            state
        else
            let s' = setIndexLevel (sl.indexLevel - 1) state
                |> playerMoveToStairs ~dir:Down
            in
            s'

let playerGoDown state =
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentLevel state) p |> isTerrainOf StairsDown |> not then
        state
    else
        let sl = state.stateLevels in
        if sl.indexLevel = List.length sl.levels - 1 then
            mapGen state
            |> playerMoveToStairs ~dir:Up
            |> playerAddMapKnowledgeEmpty
            |> playerUpdateMapKnowledge
        else
            let s' = setIndexLevel (sl.indexLevel + 1) state
                |> playerMoveToStairs ~dir:Up
            in
            s'

let modeSelectPickup state =
    let pp = state.statePlayer.pos in
    let m = getCurrentLevel state in
    let t = Matrix.get m pp in
    let selection = selectionOfItems ~single:false DoPickup t.items in
    match t.items with
        | [] ->
            let _ = addMsg state "There's nothing to pick up here." in
            Some state
        | i::[] ->
            actionPlayer (Pickup selection.sItems) state
        | _ ->
            let mode = Selecting selection in
            Some { state with mode }

let modeSelectRead state =
    let sp = state.statePlayer in
    let readables = L.filter (itemHasFlag Readable) sp.inventory in
    if L.is_empty readables then Some state else
    let mode = Selecting (selectionOfItems ~single:true DoRead readables) in
    Some { state with mode }

let modePlaying event state = match event with
    | `Key (`ASCII 'q', _) -> print_endline "See you soon..."; None
    | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) -> actionPlayer (MoveDir west) state
    | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) -> actionPlayer (MoveDir east) state
    | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) -> actionPlayer (MoveDir north) state
    | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) -> actionPlayer (MoveDir south) state

    | `Key (`ASCII 'y', _) -> actionPlayer (MoveDir northWest) state
    | `Key (`ASCII 'u', _) -> actionPlayer (MoveDir northEast) state
    | `Key (`ASCII 'b', _) -> actionPlayer (MoveDir southWest) state
    | `Key (`ASCII 'n', _) -> actionPlayer (MoveDir southEast) state

    | `Key (`ASCII 's', _) -> actionPlayer Search state

    | `Key (`ASCII 'r', _) -> modeSelectRead state

    | `Key (`ASCII ',', _) -> modeSelectPickup state

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
        ; gold = 0
        ; hp = 613
        ; hpMax = 613
        ; inventory = []
        ; knowledgeLevels = []
        }
    in

    let stateI =
        { stateLevels
        ; statePlayer
        ; messages = Q.create ()
        ; mode = Playing
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
