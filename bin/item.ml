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

type itemList = item list

type t = item

let count = function
    | Gold t -> t
    | Scroll { stats = s; _ } -> s.count
    | Container _ -> 1

let name i =
    let count = count i in
    let mPlural = if count = 1 then "" else "s" in
    let name = match i with
        | Gold t -> "gold piece" ^ mPlural
        | Scroll { scroll_t = t; stats = is } -> "scroll" ^ mPlural ^ " of " ^
                ( match t with
                    | CreateMonster -> "create monster"
                    | MagicMapping -> "magic mapping"
                )
        | Container c -> match c.container_t with
            | Sack -> "sack"
            | Chest -> "chest"
    in
    sf "%i %s" count name

let isReadable = function
    | Container _ -> false
    | Gold _ -> false
    | Scroll _ -> true

let rnGold d =
    let den = max (12 - d) 2 in
    let mul = rn 1 (30 / den) in
    let amount = (rn 1 (d + 2)) * mul in
    Gold amount

let rnScroll () =
    let t = match rn 1 90 with
        | c when c >= 1 && c <= 45 -> CreateMonster
        | c when c >= 46 && c <= 90 -> MagicMapping
        | _ -> assert false
    in
    Scroll { scroll_t = t; stats = {count = 1} }

let random () = match rn 1 16 with
    | c when c >= 1 && c <= 16 -> rnScroll ()
    | _ -> assert false
