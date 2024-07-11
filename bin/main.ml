open Common
open Matrix

let mapSize =
    { row = 21
    ; col = 80
    }

let distanceSight = 3.17

let id a = a

let range min max = List.init (max - min + 1) (fun i -> i + min)

let contains l v = List.find_opt ((=) v) l |> Option.is_some

let listMin l =
    let first = List.hd l in
    List.fold_left (fun minSofar v -> min minSofar v) first l

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
type wall = Vertical | Horizontal

type terrain =
    | Door of stateDoor
    | Floor
    | Hallway
    | StairsDown
    | StairsUp
    | Stone
    | Unseen
    | Wall of wall

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
    ; color : ANSITerminal.style
    ; difficulty : int
    ; levelBase : int
    ; hits: hit list
    }

type creature =
    { cHp : int
    ; level : int
    ; creatureInfo : creatureInfo
    }

type occupant = Creature of creature | Boulder

type tile =
    { t : terrain
    ; occupant : occupant option
    }

type levels = tile Matrix.t list
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
    ; statePlayer : statePlayer
    (* TODO messages *)
    (* objects : list Object *)
    }

type animation =
    { dir : pos
    ; posStart : pos
    ; posCurrent : pos
    ; posEnd : pos
    ; image : string
    }

type command =
    | NoOp
    | Quit of string
    | Key of char


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

let unseenEmpty = { t = Unseen; occupant = None }

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

let isStairs t =
    t.t = StairsUp || t.t = StairsDown

let isFloorOrStairs t =
    t.t = Floor || isStairs t

let isFloorOrStairsOpt = function
    | None -> false
    | Some t -> isFloorOrStairs t

let rec styleCharOfMap m pos t =
    let open ANSITerminal in
    if Option.is_some t.occupant then
        match Option.get t.occupant with
            | Creature c ->
                ( [Bold; c.creatureInfo.color]
                , c.creatureInfo.symbol
                )
        | Boulder -> [Bold; white], "0"
    else
    let c = match t.t with
    | Stone -> " "
    | Unseen -> " "
    | Hallway -> "#"
    | Floor -> "."
    | Door Closed -> "+"
    | Door Open when atNorth m pos |> isFloorOrStairsOpt -> "|"
    | Door Open when atSouth m pos |> isFloorOrStairsOpt -> "|"
    | Door Open -> "-"
    | Door Hidden -> "*" (* TODO styleCharOfMap m pos { t = Stone } *)
    | StairsUp -> "<"
    | StairsDown -> ">"
    | Wall Horizontal -> "-"
    | Wall Vertical -> "|"
    in
    [white], c


let stringOfStyleChar (style, c) =
    ANSITerminal.sprintf style "%s" c

let applyAnimatedTiles animationLayer _ m =
    animationLayer
    |> List.fold_left
        ( fun m (pos, styledChar) ->
            Matrix.set styledChar pos m
        )
        m

let view animationLayer state =
    let p = state.statePlayer.pos in
    let map =
        getCurrentLevelKnowledge state
        |> Matrix.mapI styleCharOfMap
        |> Matrix.map stringOfStyleChar
        |> Matrix.set "@" p
        |> applyAnimatedTiles animationLayer state
        |> Matrix.raw
        |> List.map (String.concat "")
        |> String.concat "\n"
    in
    Format.sprintf
{| HP: %i

%s|} state.statePlayer.hp map

let render ?(animationLayer=[]) state =
    let toPrint = view animationLayer state in
    let _, sy = ANSITerminal.size () in
    ANSITerminal.set_cursor 1 (sy - mapSize.row - 2);
    ANSITerminal.erase ANSITerminal.Below;

    Format.printf "%s%!" toPrint

let rec animate state ?(animationLayer=[]) a =
    let animationLayer' =
        if a.posCurrent = a.posStart then
            []
        else
            (a.posCurrent, a.image)::animationLayer
    in
    Unix.sleepf 0.05;
    render ~animationLayer:animationLayer' state;

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
    | _::[] -> (match t.t with Wall _ when prev.t = Hallway -> false | _ -> true)
    | hd::tl -> match t.occupant with
        | Some Boulder -> false
        | Some (Creature c) ->
            rayCanHitTarget m t tl
            (* TODO large occupants *)
        | None -> match t.t with
            | Floor | Hallway
            | StairsDown | StairsUp
            | Door Open -> rayCanHitTarget m t tl
            | Door Closed | Door Hidden -> false
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
    if room.posNW.row <= 1 || room.posNW.col <= 1
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
        | { occupant = None; t = t} ->
            ( match t with
                | Door Open -> true
                | Door Closed | Door Hidden -> false
                | Floor -> true
                | Hallway -> true
                | StairsDown -> true
                | StairsUp  -> false
                | Stone -> false
                | Unseen -> false
                | Wall Horizontal | Wall Vertical -> false
            )

let placeCreature ~room state =
    let m = getCurrentLevel state in
    let pp = Some (state.statePlayer.pos) in
    let spawnPositions =
        allMapPositions
        |> List.filter (fun p -> canSpawnHere ~forbidPos:pp m p)
    in
    let positionsOutOfView = spawnPositions|> List.filter
        ( fun p -> not (playerCanSee state p) ) in

    let (iv, oov) = match room with
        | None -> spawnPositions, positionsOutOfView
        | Some r ->
              List.filter (isInRoom r) spawnPositions
            , List.filter (isInRoom r) positionsOutOfView
    in
    let creaturePos =  match iv, oov with
        | [], [] -> None
        | _, (_::_ as oov) -> Some (rnItem oov)
        | pOk, _ -> Some (rnItem pOk)

    in
    match creaturePos with
        | None -> state
        | Some p ->
            let creature =
                { cHp = 7 (* TODO pick creatures *)
                ; level = 15
                ; creatureInfo =
                    { symbol = "D"
                    ; color = ANSITerminal.red
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
            | Door Closed | Door Hidden | Door Open -> true
            | Floor -> false
            | Hallway -> allowHallway
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
    | Floor | StairsUp | StairsDown | Hallway -> true
    | Door Open -> true
    | Door Closed | Door Hidden -> false
    | Stone -> false
    | Unseen -> false
    | Wall Horizontal | Wall Vertical -> false


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
    let cl' = Matrix.set t' p cl in
    setCurrentLevel cl' state


let playerAttackMelee t p c state =
    (* TODO base on stats *)
    creatureAddHp (-5) t p c state

let playerMove (r, c) s =
    let p = s.statePlayer.pos in
    let t = getCurrentLevel s in
    let pn = { row = p.row + r; col = p.col + c } in

    match Matrix.get t pn with
    | { t = Door Closed; _ } ->
        (* TODO make chance based on stats *)
        if rn 0 2 = 0 then
            s
        else
            let tn = Matrix.set
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

let playerSearch state =
    (* TODO base search success on stats *)
    let currentLevel = getCurrentLevel state in
    let hiddenDoorsAround =
        posAround state.statePlayer.pos
        |> List.filter (fun pa -> Matrix.get currentLevel pa |> isTerrainOf (Door Hidden) )
    in
    let terrain' = List.fold_right
        ( fun d m ->
            if (rn 0 3 > 0) then
                m
            else
                Matrix.set
                    { t = Door Closed
                    ; occupant = None
                    }
                    d m
        ) hiddenDoorsAround currentLevel
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

let rollMiss threshold addSides =
    rn 1 (20 + addSides) >= threshold

let rollReducedDamage ac damage =
    if ac >= 0 then damage else
    (rn ac (-1)) + damage |> min 1

let creatureAttackMelee c p state =
    if p = state.statePlayer.pos then
        let hitThreshold = getHitThreshold 0 c.level in
        (* ^TODO player AC *)
        c.creatureInfo.hits
        |> List.filter_map (function | Melee hm -> Some hm | _ -> None)
        |> List.mapi (fun i v -> i, v)
        |> List.fold_left (fun state' (addSides, hm) ->
            if rollMiss hitThreshold addSides then
                state'
            else
            let effectSize =
                rollEffectSize hm.hitStats.roll
                |> rollReducedDamage 0
                (* ^TODO player AC *)
            in

            playerAddHp (-effectSize) state'
            ) state

    else
        let _ = assert false in
        state

let getImageForAnimation t dir =
    let open ANSITerminal in

    let color = match t with
    | Physical -> white
    | Fire -> red
    in

    let c = match dir with
    | _ when dir.row = 0 -> "-"
    | _ when dir.col = 0 -> "|"
    | _ when dir.row = dir.col -> "\\"
    | _ when dir.row <> dir.col -> "/"
    | _ -> assert false
    in

    ([Bold; color], c) |> stringOfStyleChar

let creatureAttackRanged c cp tp state =
    let rec processPath effectSize cp pd pTarget state =
        let cp' = posAdd cp pd in
        let pp = state.statePlayer.pos in
        let m = getCurrentLevel state in

        let state' = match Matrix.get m cp' with
            | { occupant = Some Boulder; _ } -> state (* TODO rays/vs weapons *)
            | { occupant = Some (Creature c); _ } as t ->
                    creatureAddHp (-effectSize) t cp' c state
                    (* ^TODO resistances *)

            | { occupant = None; _ } ->
                    if cp' <> pp then
                        state
                    else
                        playerAddHp (-effectSize) state
        in

        (* TODO stop ray when ranged sufficiently reduced
        -- not necessarily on target hit
        -- TODO Reflections
        *)
        if cp' = pTarget then
            state'
        else
            processPath effectSize cp' pd pTarget state'
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
            animate state' animation;
            processPath effectSize cp pDiff tp state'
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
            animateCreature c p state'
        | _ -> state'
    ) state (getCurrentLevel state)

let maybeAddCreature state =
    if rn 0 50 > 0 then state else
    placeCreature ~room:None state

let playerCheckHp (state, c) =
    let sp = state.statePlayer in
    if sp.hp <= 0 then
        (state, Quit "You died...")
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
    , NoOp
    )
    |> playerCheckHp

let terrainAddRoom m room =
    let rp = getRoomPositions room in
    let withFloor = List.fold_left
        ( fun m p ->
            Matrix.set
                { t = Floor
                ; occupant = None
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
            Matrix.set { t = Wall alignment; occupant = None } p m
        )
        withFloor
        outline
    in
    List.fold_left
        ( fun m d ->
            let stateDoor = match rn 0 2 with
                | 0 -> Hidden
                | 1 -> Closed
                | 2 -> Open
                | _ -> assert false
            in
            Matrix.set
                { t = Door stateDoor
                ; occupant = None
                }
                d m
        ) withWalls room.doors

let terrainAddRooms rooms t =
    List.fold_right (fun r m -> terrainAddRoom m r) rooms t

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
            let m = List.fold_right
                ( fun p m ->
                    Matrix.set
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
            if Matrix.get m p |> isStairs then
                terrainAddStairs ~dir rooms m
            else
                Matrix.set stairs p m

let rec placeCreatures rooms state =
    match rooms with
        | [] -> state
        | r::ro ->
            let s' = placeCreature ~room:(Some r) state in
            if rn 0 2 = 0 then
                placeCreatures ro s'
            else
                s'

let playerMoveToStairs ~dir state =
    let stairType = match dir with
        | Up -> StairsUp
        | Down -> StairsDown
    in
    let posStairs = getPosTerrain (getCurrentLevel state) stairType
    in
    let statePlayer = { state.statePlayer with pos = posStairs } in
    { state with statePlayer }


let mapGen state =
    let rooms = roomsGen () in
    let terrain = Matrix.fill mapSize { t = Stone; occupant = None }
        |> terrainAddRooms rooms
        |> terrainAddStairs ~dir:Up rooms
        |> terrainAddStairs ~dir:Down rooms
        |> terrainAddHallways rooms
    in
    addLevel terrain state
    |> playerMoveToStairs ~dir:Up
    |> placeCreatures rooms


let playerGoUp state =
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentLevel state) p |> isTerrainOf StairsUp |> not then
        (state, NoOp)
    else
        let sl = state.stateLevels in
        if sl.indexLevel = 0 then
            (state, NoOp)
        else
            let s' = setIndexLevel (sl.indexLevel - 1) state
                |> playerMoveToStairs ~dir:Down
            in
            (s', NoOp)

let playerGoDown state =
    let p = state.statePlayer.pos in
    if Matrix.get (getCurrentLevel state) p |> isTerrainOf StairsDown |> not then
        (state, NoOp)
    else
        let sl = state.stateLevels in
        if sl.indexLevel = List.length sl.levels - 1 then
            ( mapGen state
                |> playerMoveToStairs ~dir:Up
                |> playerAddMapKnowledgeEmpty
                |> playerUpdateMapKnowledge
            , NoOp
            )
        else
            let s' = setIndexLevel (sl.indexLevel + 1) state
                |> playerMoveToStairs ~dir:Up
            in
            (s', NoOp)


let update event state = match event with
    | Key 'q' -> (state, Quit "See you soon...")
    | Key 'h' -> playerAction (MoveDelta (0, -1)) state
    | Key 'l' -> playerAction (MoveDelta (0,  1)) state
    | Key 'k' -> playerAction (MoveDelta (-1, 0)) state
    | Key 'j' -> playerAction (MoveDelta (1,  0)) state

    | Key 'y' -> playerAction (MoveDelta (-1, -1)) state
    | Key 'u' -> playerAction (MoveDelta (-1,  1)) state
    | Key 'b' -> playerAction (MoveDelta (1,  -1)) state
    | Key 'n' -> playerAction (MoveDelta (1,   1)) state

    | Key 's' -> playerAction Search state

    | Key '<' -> playerGoUp state
    | Key '>' -> playerGoDown state
    | Key _ -> (state, NoOp)
    | NoOp -> assert false
    | Quit _ -> assert false


let getChar () =
    (* TODO lwt just for read_char? *)
    Lwt_main.run Lwt_io.(read_char stdin)

let rec loop (state, command) =
    render state;

    match command with
    | NoOp ->
        let c = getChar () in
        update (Key c) state
        |> loop
    | Quit s ->
        Format.printf "\n%s\n" s;
        Terminal.restoreAttributes ()
    | _ -> update command state |> loop

let stateInitial =
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
        }
    in
    mapGen stateI
    |> playerMoveToStairs ~dir:Up
    |> playerAddMapKnowledgeEmpty
    |> playerUpdateMapKnowledge

let () =
    Terminal.setup ();
    loop (stateInitial, NoOp)
