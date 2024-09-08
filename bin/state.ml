module C = Common

type level_t =
    | Dungeon
    | Garden of Random_.uid (* mitras id *)

type level =
    { map : Map.t
    ; rooms : Map.room list
    ; level_t : level_t
    }

type levels =
    { indexLevel : int
    ; levels : level list
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
    ; weaponWielded : Item.weapon option
    ; inventory : Item.t list
    ; inventoryWeightMax : int
    ; status : C.status list
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
    ; onComplete : onSelectItemsComplete
    }

type selection =
    | SelectDir of selectDir
    | SelectItems of selectItems

type mode =
    | Dead
    | DisplayText of string list
    | Playing
    | Selecting of selection

type t =
    { levels : levels
    ; player : player
    ; messages : string Queue.t
    ; mode : mode
    ; turns : int
    }

let msgAdd state s = Queue.push s state.messages

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
