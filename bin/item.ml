module N = Notty
module A = N.A

module C = Common
module R = Random_

type weapon =
    { name : string
    ; color : A.color
    ; damage : R.roll
    ; price : int
    ; freqRel : int
    ; weight : int
    }

type stats =
    { count : int
    }

type comestible =
    { name : string
    ; color : A.color
    ; freqRel : int
    ; price : int
    ; weight : int
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
    | Dig
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
    ; weight : int
    }

type container =
    { container_t : container_t
    ; items : item list
    }

and item =
    | Comestible of comestible
    | Container of container
    | Corpse of corpse
    | Gold of int
    | Potion of potion
    | Rock of int
    | Scroll of scroll
    | Weapon of weapon
    | Wand of wand

type itemList = item list

type t = item

let count = function
    | Comestible _ -> 1
    | Container _ -> 1
    | Corpse _ -> 1
    | Gold t -> t
    | Potion { stats = s; _ } -> s.count
    | Rock t -> t
    | Scroll { stats = s; _ } -> s.count
    | Weapon _ -> 1
    | Wand _ -> 1

let weight = function
    | Comestible c -> c.weight
    | Container _ -> assert false
    | Corpse c -> c.weight
    | Gold _ -> 0
    | Potion { stats = s; _ } -> 20 * s.count
    | Rock t -> 10 * t
    | Scroll { stats = s; _ } -> 5 * s.count
    | Weapon w -> w.weight
    | Wand _ -> 7

let name ?(mPlural="") = function
    | Comestible c -> c.name ^ mPlural
    | Corpse c -> c.name ^ " corpse"
    | Gold _ -> "gold piece" ^ mPlural
    | Potion p -> "potion" ^ mPlural ^ " of " ^
        ( match p.potion_t with
            | Healing -> "healing"
            | HealingExtra -> "extra healing"
            | HealingFull -> "bottled fairy"
            | Sickness -> "sickness"
        )
    | Rock _ -> "rock" ^ mPlural
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
            | Dig -> "digging"
            | Fire -> "fire"
            | MagicMissile -> "magic missile"
            | Striking -> "striking"
        )

let nameDisplay i =
    let count = count i in
    let mPlural = if count = 1 then "" else "s" in
    C.sf "%i %s" count (name ~mPlural i)

let getPriceBase = function
    | Comestible c -> c.price
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
    | Rock _ -> 0
    | Scroll s ->
        ( match s.scroll_t with
            | CreateMonster -> 200
            | MagicMapping -> 100
            | Teleport -> 100
        )
    | Weapon w -> w.price
    | Wand w ->
        ( match w.wand_t with
            | Dig -> 150
            | Fire -> 175
            | MagicMissile -> 150
            | Striking -> 150
        )

let getPriceTrade i =
    let pb = match i with
        | Wand w -> if 0 = w.charges then 0 else getPriceBase i
        | _ -> getPriceBase i
    in
    pb / 2

let getPriceShop i = getPriceBase i * 4 / 3

let isQuaffable = function
    | Comestible _ -> false
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> true
    | Rock _ -> false
    | Scroll _ -> false
    | Weapon _ -> false
    | Wand _ -> false

let isReadable = function
    | Comestible _ -> false
    | Container _ -> false
    | Corpse _ -> false
    | Gold _ -> false
    | Potion _ -> false
    | Rock _ -> false
    | Scroll _ -> true
    | Weapon _ -> false
    | Wand _ -> false

let isZappable = function
    | Wand _ -> true
    | _ -> false

let rock n = Rock n

let comestibles =
    [   { name = "apple"
        ; color = A.red
        ; price = 7
        ; freqRel = 15
        ; weight = 2
        }
    ;   { name = "carrot"
        ; color = A.yellow
        ; price = 7
        ; freqRel = 15
        ; weight = 2
        }
    ]

let runedBroadswordInfo =
    { name = "runed broadsword"
    ; color = A.white
    ; damage = { rolls = 2; sides = 6 }
    ; price = 10
    ; freqRel = 1
    ; weight = 70
    }

let runedBroadsword = Weapon runedBroadswordInfo

let weapons =
    [   { name = "dagger"
        ; color = A.cyan
        ; damage = { rolls = 1; sides = 4 }
        ; price = 4
        ; freqRel = 30
        ; weight = 10
        }
    ;   { name = "club"
        ; color = C.brown
        ; damage = { rolls = 1; sides = 6 }
        ; price = 3
        ; freqRel = 12
        ; weight = 30
        }
    ; runedBroadswordInfo
    ]

let rnComestible () =
    let freq = List.map (fun c -> c, c.freqRel) comestibles in
    Comestible (R.relative freq)

let rnWeapon () =
    let freq = List.map (fun (w : weapon) -> w, w.freqRel) weapons in
    Weapon (R.relative freq)

let rnGold d =
    let den = max (12 - d) 2 in
    let mul = R.rn 1 (30 / den) in
    let base = R.rn 1 (d + 2) in
    Gold (base * mul)

let rnPotion () =
    let freq =
        [ Healing, 115
        ; HealingExtra, 45
        ; HealingFull, 10
        ; Sickness, 40
        ]
    in
    let t = R.relative freq in
    Potion { potion_t = t; stats = {count = 1} }

let rnScroll () =
    let freq =
        [ CreateMonster, 45
        ; MagicMapping, 45
        ; Teleport, 55
        ]
    in
    let t = R.relative freq in
    Scroll { scroll_t = t; stats = {count = 1} }

let rnWand () =
    let freq =
        [ Dig, 11
        ; Fire, 8
        ; MagicMissile, 10
        ; Striking, 15
        ]
    in
    let t = R.relative freq in
    Wand { wand_t = t; charges = R.rn 4 8 }

let random () =
    let freq =
        [ rnComestible,  8
        ; rnPotion,     16
        ; rnScroll,     16
        ; rnWeapon,     10
        ; rnWand,        4
        ]
    in
    let t = R.relative freq in
    t ()

let isComestible = function
    | Comestible _ -> true
    | _ -> false

let isThrowable = function
    | Comestible _
    | Weapon _
    -> true
    | Container _
    | Corpse _
    | Gold _
    | Potion _
    | Rock _
    | Scroll _
    | Wand _
    -> false

let isWeapon = function
    | Weapon _ -> true
    | _ -> false

let toWeapon = function
    | Weapon w -> w
    | _ -> assert false

let getWeaponsByDamage l =
    List.filter isWeapon l
    |> List.map toWeapon
    |> List.sort (fun w1 w2 -> R.rollCompare w2.damage w1.damage)

let getWeaponMostDamaging l =
    let bd = getWeaponsByDamage l in
    List.nth_opt bd 0

let isCorpse = function
    | Corpse _ -> true
    | _ -> false

let turnsCorpseUnfresh = 6
let turnsCorpseUnhealthy = 25
let turnsCorpseTainted = 50
let turnsCorpseRot = 100
let corpseAgeZombie = turnsCorpseTainted

type corpseFreshness =
    | Fresh
    | Unfresh
    | Unhealthy
    | Tainted

let corpseFreshness c turns =
    let age = turns - c.turnDeceased in

    match c with
    | _ when age < turnsCorpseUnfresh -> Fresh
    | _ when age < turnsCorpseUnhealthy -> Unfresh
    | _ when age < turnsCorpseTainted -> Unhealthy
    | _ -> Tainted

let mkCorpse name color weight t =
    let lenNoZombie = String.length name - String.length "zombie" - 1 in

    let addAge, name = match String.ends_with ~suffix:"zombie" name with
        | true -> corpseAgeZombie, String.sub name 0 lenNoZombie
        | false -> 0, name
    in
    Corpse
        { name
        ; color
        ; turnDeceased = t - addAge
        ; weight
        }

let rotCorpses turns l =
    List.fold_left
        ( fun items i -> match i with
            | Corpse c when turns - c.turnDeceased > turnsCorpseRot -> items
            | i -> i::items
        ) [] (List.rev l)
