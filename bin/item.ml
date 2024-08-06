module N = Notty
module A = N.A

module C = Common

type weapon =
    { name : string
    ; color : A.color
    ; damage : C.roll
    ; price : int
    ; freqRel : int
    }

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

type wand_t =
    | Fire
    | MagicMissile
    | Striking

type wand =
    { charges : int
    ; wand_t : wand_t
    }

type container_t =
    | Sack
    | Chest

type corpse =
    { name : string
    ; color : A.color
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
    | Weapon of weapon
    | Wand of wand

type itemList = item list

type t = item

let count = function
    | Container _ -> 1
    | Corpse _ -> 1
    | Gold t -> t
    | Potion { stats = s; _ } -> s.count
    | Scroll { stats = s; _ } -> s.count
    | Weapon _ -> 1
    | Wand _ -> 1

let name ?(mPlural="") = function
    | Corpse c -> c.name ^ " corpse"
    | Gold _ -> "gold piece" ^ mPlural
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
    | Container c ->
        ( match c.container_t with
        | Sack -> "sack"
        | Chest -> "chest"
        )
    | Weapon w -> w.name
    | Wand w -> "wand"^ mPlural ^ " of " ^
        ( match w.wand_t with
            | Fire -> "fire"
            | MagicMissile -> "magic missile"
            | Striking -> "striking"
        )

let nameDisplay i =
    let count = count i in
    let mPlural = if count = 1 then "" else "s" in
    C.sf "%i %s" count (name ~mPlural i)

let getPriceBase = function
    | Container c ->
        ( match c.container_t with
            | Chest -> 16
            | Sack -> 2
        )
    | Corpse _ -> 0
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
    | Weapon w -> w.price
    | Wand w ->
        ( match w.wand_t with
            | Fire -> 175
            | MagicMissile -> 150
            | Striking -> 150
        )

let isQuaffable = function
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> true
    | Scroll _ -> false
    | Weapon _ -> false
    | Wand _ -> false

let isReadable = function
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> false
    | Scroll _ -> true
    | Weapon _ -> false
    | Wand _ -> false

let isZappable = function
    | Wand w -> true
    | _ -> false

let weapons =
    [   { name = "dagger"
        ; color = A.cyan
        ; damage = { rolls = 1; sides = 4 }
        ; price = 4
        ; freqRel = 30
        }
    ;   { name = "club"
        ; color = C.brown
        ; damage = { rolls = 1; sides = 6 }
        ; price = 3
        ; freqRel = 12
        }
    ]

let rnWeapon () =
    let freq = List.map (fun w -> w, w.freqRel) weapons in
    Weapon (C.rnRelative freq)

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

let rnWand () =
    let freq =
        [ Fire, 8
        ; MagicMissile, 10
        ; Striking, 15
        ]
    in
    let t = C.rnRelative freq in
    Wand { wand_t = t; charges = C.rn 4 8 }

let random () =
    let freq =
        [ rnPotion, 16
        ; rnScroll, 16
        ; rnWeapon, 10
        ; rnWand,    4
        ]
    in
    let t = C.rnRelative freq in
    t ()

let isWeapon = function
    | Weapon _ -> true
    | _ -> false

let toWeapon = function
    | Weapon w -> w
    | _ -> assert false

let getWeaponsByDamage l =
    List.filter isWeapon l
    |> List.map toWeapon
    |> List.sort (fun w1 w2 -> C.rollCompare w2.damage w1.damage)

let getWeaponMostDamaging l =
    let bd = getWeaponsByDamage l in
    List.nth_opt bd 0

let isCorpse = function
    | Corpse _ -> true
    | _ -> false

let turnsCorpseRot = 100
let corpseAgeZombie = 50

let mkCorpse name color t =
    let addAge = if String.ends_with ~suffix:"zombie" name then corpseAgeZombie else 0 in
    Corpse
        { name
        ; color
        ; turnDeceased = t - addAge
        }

let rotCorpses turns l =
    List.fold_left
        ( fun items i -> match i with
            | Corpse c when turns - c.turnDeceased > turnsCorpseRot -> items
            | i -> i::items
        ) [] (List.rev l)
