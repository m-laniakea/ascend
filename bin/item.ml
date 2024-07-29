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

type container =
    { container_t : container_t
    ; items : item list
    }

and item =
    | Container of container
    | Gold of int
    | Potion of potion
    | Scroll of scroll

type itemList = item list

type t = item

let count = function
    | Container _ -> 1
    | Gold t -> t
    | Potion { stats = s; _ } -> s.count
    | Scroll { stats = s; _ } -> s.count

let name i =
    let count = count i in
    let mPlural = if count = 1 then "" else "s" in
    let name = match i with
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
    | Gold i -> i
    | Potion p ->
        ( match p.potion_t with
            | Healing -> 200
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
    | Gold _ -> false
    | Potion _ -> true
    | Scroll _ -> false

let isReadable = function
    | Container _ -> false
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
