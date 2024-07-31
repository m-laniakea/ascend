module C = Common

type stats =
    { count : int
    }

type scroll_t =
    | CreateMonster
    | MagicMapping
    | Teleport

type scroll =
    { stats : stats
    ; scroll_t : scroll_t
    }

type potion_t =
    | Healing
    | HealingExtra
    | HealingFull
    | Sickness

type potion =
    { stats : stats
    ; potion_t : potion_t
    }

type container_t =
    | Sack
    | Chest

type corpse =
    { creature : Creature.info
    ; turnDeceased : int
    }

type container =
    { container_t : container_t
    ; items : item list
    }

and item =
    | Container of container
    | Corpse of corpse
    | Gold of int
    | Potion of potion
    | Scroll of scroll

type itemList = item list

type t = item

let count = function
    | Container _ -> 1
    | Corpse _ -> 1
    | Gold t -> t
    | Potion { stats = s; _ } -> s.count
    | Scroll { stats = s; _ } -> s.count

let name i =
    let count = count i in
    let mPlural = if count = 1 then "" else "s" in
    let name = match i with
        | Corpse c -> c.creature.name ^ " corpse"
        | Gold t -> "gold piece" ^ mPlural
        | Potion p -> "potion" ^ mPlural ^ " of " ^
            ( match p.potion_t with
                | Healing -> "healing"
                | HealingExtra -> "extra healing"
                | HealingFull -> "full healing"
                | Sickness -> "sickness"
            )

        | Scroll s -> "scroll" ^ mPlural ^ " of " ^
            ( match s.scroll_t with
                | CreateMonster -> "create monster"
                | MagicMapping -> "magic mapping"
                | Teleport -> "teleportation"
            )
        | Container c -> match c.container_t with
            | Sack -> "sack"
            | Chest -> "chest"
    in
    C.sf "%i %s" count name

let getPriceBase = function
    | Container c ->
        ( match c.container_t with
            | Chest -> 16
            | Sack -> 2
        )
    | Corpse c -> 0
    | Gold i -> i
    | Potion p ->
        ( match p.potion_t with
            | Healing -> 20
            | HealingExtra -> 100
            | HealingFull -> 200
            | Sickness -> 50

        )
    | Scroll s ->
        ( match s.scroll_t with
            | CreateMonster -> 200
            | MagicMapping -> 100
            | Teleport -> 100
        )

let isQuaffable = function
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> true
    | Scroll _ -> false

let isReadable = function
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> false
    | Scroll _ -> true

let rnGold d =
    let den = max (12 - d) 2 in
    let mul = C.rn 1 (30 / den) in
    let base = C.rn 1 (d + 2) in
    Gold (base * mul)

let rnPotion () =
    let freq =
        [ Healing, 115
        ; HealingExtra, 45
        ; HealingFull, 10
        ; Sickness, 40
        ]
    in
    let t = C.rnRelative freq in
    Potion { potion_t = t; stats = {count = 1} }

let rnScroll () =
    let freq =
        [ CreateMonster, 45
        ; MagicMapping, 45
        ; Teleport, 55
        ]
    in
    let t = C.rnRelative freq in
    Scroll { scroll_t = t; stats = {count = 1} }

let random () = match C.rn 1 32 with
    | c when c >= 1  && c <= 16 -> rnPotion ()
    | c when c >= 16 && c <= 32 -> rnScroll ()
    | _ -> assert false

let isCorpse = function
    | Corpse _ -> true
    | _ -> false

let turnsCorpseRot = 100
let corpseAgeZombie = 50

let mkCorpse (ci : Creature.info) t =
    let addAge = if String.ends_with ~suffix:"zombie" ci.name then corpseAgeZombie else 0 in
    Corpse
        { creature = ci
        ; turnDeceased = t - addAge
        }

let rotCorpses turns l =
    List.fold_left
        ( fun items i -> match i with
            | Corpse c when turns - c.turnDeceased > turnsCorpseRot -> items
            | i -> i::items
        ) [] (List.rev l)
