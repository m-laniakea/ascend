open Common

type stats =
    { count : int
    }

type scroll_t =
    | CreateMonster
    | MagicMapping

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
            )
        | Container c -> match c.container_t with
            | Sack -> "sack"
            | Chest -> "chest"
    in
    sf "%i %s" count name

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
    let mul = rn 1 (30 / den) in
    let amount = (rn 1 (d + 2)) * mul in
    Gold amount

let rnPotion () =
    let t = match rn 1 210 with
        | c when c >= 1   && c <= 115 -> Healing
        | c when c >= 115 && c <= 160 -> HealingExtra
        | c when c >= 160 && c <= 170 -> HealingFull
        | c when c >= 170 && c <= 210 -> Sickness
        | _ -> assert false
    in
    Potion { potion_t = t; stats = {count = 1} }

let rnScroll () =
    let t = match rn 1 90 with
        | c when c >= 1  && c <= 45 -> CreateMonster
        | c when c >= 45 && c <= 90 -> MagicMapping
        | _ -> assert false
    in
    Scroll { scroll_t = t; stats = {count = 1} }

let random () = match rn 1 32 with
    | c when c >= 1  && c <= 16 -> rnPotion ()
    | c when c >= 16 && c <= 32 -> rnScroll ()
    | _ -> assert false
