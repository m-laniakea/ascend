module C = Common

type level_t =
    | Dungeon
    | Final
    | Garden of Random_.uid (* mitras id *)

type level =
    { map : Map.t
    ; rooms : Map.room list
    ; level_t : level_t
    }

type levels =
    { indexLevel : int
    ; levels : level list
    ; hasBigroom : bool
    ; hasGarden : bool
    }

type player =
    { attributes : Creature.attributes list
    ; pos : Position.t
    ; gold : int
    ; hp : int
    ; hpMax : int
    ; level : int
    ; xp : int
    ; acBonus : int
    ; weaponWielded : Item.t option
    ; inventory : Item.t list
    ; inventoryWeightMax : int
    ; status : C.status list
    ; timesKilled : int
    ; knowledgeLevels : level list
    ; turnHealthWarned : int
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
    | DoClose
    | DoThrow of C.selectionItem
    | DoZap of C.selectionItem

type selectItems =
    { sItems : C.selectionItem list
    ; single : bool
    ; howMany : C.howMany
    ; onComplete : onSelectItemsComplete
    }

type selection =
    | SelectDir of selectDir
    | SelectItems of selectItems

type displayText =
    { scroll : int
    ; text : string list
    }

type mode =
    | Dead
    | DisplayText of displayText
    | Farview of Position.t
    | Playing
    | Selecting of selection
    | Victory

type stateEndgame =
    { timesGnilsogSlain : int
    ; gnilsogAlive : bool
    ; nextHarassment : int
    }

type endgame =
    | BeforeEndgame
    | Endgame of stateEndgame

type t =
    { levels : levels
    ; player : player
    ; endgame : endgame
    ; messages : string Queue.t
    ; mode : mode
    ; turns : int
    }

let displayText text =
    DisplayText
    { scroll = 0
    ; text
    }

let msgAdd state s = Queue.push s state.messages

let msgAddSeen state ~canSee s =
    if canSee then
        msgAdd state s
    else
        ()

let msgHearNotSeen state ~canSee s =
    if canSee then
        ()
    else
        msgAdd state ("You hear " ^ s)

let getKnowledgeCurrentLevel state =
    let sl = state.levels in
    List.nth state.player.knowledgeLevels sl.indexLevel

let getKnowledgeCurrentMap state =
    (getKnowledgeCurrentLevel state).map

let setKnowledgeCurrentMap m state =
    let sp = state.player in
    let i = state.levels.indexLevel in
    let ckl = getKnowledgeCurrentLevel state in
    let knowledgeLevels = C.listSet i { ckl with map = m } sp.knowledgeLevels in
    let player = { state.player with knowledgeLevels} in
    { state with player }

let incTurns state =
    { state with turns = state.turns + 1 }
