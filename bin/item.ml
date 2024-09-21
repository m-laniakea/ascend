module N = Notty
module A = N.A

module C = Common
module R = Random_

type stackable =
    | NonStackable
    | Stack of int

type weapon =
    { name : string
    ; color : A.color
    ; damage : R.roll
    ; price : int
    ; freqRel : int
    ; stackable : bool
    ; weight : int
    }

type stats =
    { stack : stackable
    }

type comestible =
    { name : string
    ; color : A.color
    ; freqRel : int
    ; price : int
    ; weight : int
    }

type scroll =
    | CreateMonster
    | MagicMapping
    | Teleport

type potion =
    | Healing
    | HealingExtra
    | HealingFull
    | Sickness

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

and item_t =
    | Comestible of comestible
    | Container of container
    | Corpse of corpse
    | Gold
    | Potion of potion
    | Rock
    | Scroll of scroll
    | Weapon of weapon
    | Wand of wand

and item =
    { t : item_t
    ; stats : stats
    }

type itemList = item list

type t = item

let count item = match item.stats.stack with
    | NonStackable -> 1
    | Stack n -> n

let weight item =
    let count = count item in
    let weightSingle = match item.t with
        | Comestible c -> c.weight
        | Container _ -> assert false
        | Corpse c -> c.weight
        | Gold -> 0 (* TODO 0 because we cannot drop gold yet *)
        | Potion _ -> 20
        | Rock -> 10
        | Scroll _ -> 5
        | Weapon w -> w.weight
        | Wand _ -> 7
    in
    weightSingle * count

let name ?(mPlural="") item = match item.t with
    | Comestible c -> c.name ^ mPlural
    | Corpse c -> c.name ^ " corpse"
    | Gold -> "gold piece" ^ mPlural
    | Potion p -> "potion" ^ mPlural ^ " of " ^
        ( match p with
            | Healing -> "healing"
            | HealingExtra -> "extra healing"
            | HealingFull -> "bottled fairy"
            | Sickness -> "sickness"
        )
    | Rock -> "rock" ^ mPlural
    | Scroll s -> "scroll" ^ mPlural ^ " of " ^
        ( match s with
            | CreateMonster -> "create monster"
            | MagicMapping -> "magic mapping"
            | Teleport -> "teleportation"
        )
    | Container c ->
        ( match c.container_t with
        | Sack -> "sack"
        | Chest -> "chest"
        )
    | Weapon w -> w.name ^ mPlural
    | Wand w -> "wand" ^ mPlural ^ " of " ^
        ( match w.wand_t with
            | Dig -> "digging"
            | Fire -> "fire"
            | MagicMissile -> "magic missile"
            | Striking -> "striking"
        )

let nameDisplay item =
    let count = count item in
    let mPlural = C.plural count in
    let name = name ~mPlural item in
    C.sf "%i %s" count name

let getPriceBase item =
    ( match item.t with
    | Comestible c -> c.price
    | Container c ->
        ( match c.container_t with
            | Chest -> 16
            | Sack -> 2
        )
    | Corpse _ -> 0
    | Gold -> 1
    | Potion p ->
        ( match p with
            | Healing -> 20
            | HealingExtra -> 100
            | HealingFull -> 200
            | Sickness -> 50

        )
    | Rock -> 0
    | Scroll s ->
        ( match s with
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
    )
    * (count item)

let getPriceTrade i =
    let pb = match i.t with
        | Wand w -> if 0 = w.charges then 0 else getPriceBase i
        | _ -> getPriceBase i
    in
    pb / 2

let getPriceShop i = getPriceBase i * 4 / 3

let isQuaffable item = match item.t with
    | Comestible _ -> false
    | Container _ -> false
    | Corpse _ -> false
    | Gold -> false
    | Potion _ -> true
    | Rock -> false
    | Scroll _ -> false
    | Weapon _ -> false
    | Wand _ -> false

let isReadable item = match item.t with
    | Comestible _ -> false
    | Container _ -> false
    | Corpse _ -> false
    | Gold -> false
    | Potion _ -> false
    | Rock -> false
    | Scroll _ -> true
    | Weapon _ -> false
    | Wand _ -> false

let isZappable item = match item.t with
    | Wand _ -> true
    | _ -> false

let rock n =
    let stats = { stack = Stack n } in
    { t = Rock
    ; stats
    }

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

let infoRunedBroadsword =
    { name = "runed broadsword"
    ; color = A.white
    ; damage = { rolls = 2; sides = 6 }
    ; price = 10
    ; freqRel = 1
    ; stackable = false
    ; weight = 70
    }

let mkWeapon info =
    let stack = if info.stackable then Stack 1 else NonStackable in
    let stats = { stack } in

    { t = Weapon info
    ; stats
    }

let scepterOfYorel =
    mkWeapon
    { name = "Scepter of Yorel"
    ; color = A.lightcyan
    ; damage = { rolls = 8; sides = 1 }
    ; price = 19960509
    ; freqRel = 0
    ; stackable = false
    ; weight = 100
    }

let runedBroadsword = mkWeapon infoRunedBroadsword

let weapons =
    [
        { name = "dagger"
        ; color = A.cyan
        ; damage = { rolls = 1; sides = 4 }
        ; price = 4
        ; freqRel = 30
        ; stackable = true
        ; weight = 10
        }
    ;
        { name = "club"
        ; color = C.brown
        ; damage = { rolls = 1; sides = 6 }
        ; price = 3
        ; freqRel = 12
        ; stackable = true
        ; weight = 30
        }
    ; infoRunedBroadsword
    ]

let rnComestible () =
    let freq = List.map (fun c -> c, c.freqRel) comestibles in

    { t = Comestible (R.relative freq)
    ; stats = { stack = Stack 1 }
    }

let rnWeapon () =
    let freq = List.map (fun (w : weapon) -> w, w.freqRel) weapons in
    mkWeapon (R.relative freq)

let rnGold d =
    let den = max (12 - d) 2 in
    let mul = R.rn 1 (30 / den) in
    let base = R.rn 1 (d + 2) in

    let stack = Stack (base * mul) in
    { t = Gold
    ; stats = { stack }
    }

let rnPotion () =
    let freq =
        [ Healing, 115
        ; HealingExtra, 45
        ; HealingFull, 10
        ; Sickness, 40
        ]
    in
    let t = R.relative freq in
    { t = Potion t
    ; stats = { stack = Stack 1 }
    }

let rnScroll () =
    let freq =
        [ CreateMonster, 45
        ; MagicMapping, 45
        ; Teleport, 55
        ]
    in
    let t = R.relative freq in
    { t = Scroll t
    ; stats = { stack = Stack 1 }
    }

let rnWand () =
    let freq =
        [ Dig, 11
        ; Fire, 8
        ; MagicMissile, 10
        ; Striking, 15
        ]
    in
    let t = R.relative freq in
    let t = Wand { wand_t = t; charges = R.rn 4 8 } in
    { t
    ; stats = { stack = NonStackable }
    }

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

let isComestible item = match item.t with
    | Comestible _ -> true
    | _ -> false

let isThrowable item = match item.t with
    | Comestible _
    | Weapon _
    -> true
    | Container _
    | Corpse _
    | Gold
    | Potion _
    | Rock
    | Scroll _
    | Wand _
    -> false

let isGold i = match i.t with
    | Gold -> true
    | _ -> false

let isWeapon i = match i.t with
    | Weapon _ -> true
    | _ -> false

let toWeapon = function
    | Weapon w -> w
    | _ -> assert false

let getWeaponsByDamage l =
    l
    |> List.filter isWeapon
    |> List.sort
        ( fun w1 w2 -> match w1.t, w2.t with
        | Weapon w1, Weapon w2 -> R.rollCompare w2.damage w1.damage
        | _ -> assert false
        )

let getWeaponMostDamaging l =
    let bd = getWeaponsByDamage l in
    List.nth_opt bd 0

let getWeaponDamage w = match w.t with
    | Weapon w -> w.damage
    | _ -> failwith "not a weapon"

let isCorpse i = match i.t with
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
    let t =
        Corpse
        { name
        ; color
        ; turnDeceased = t - addAge
        ; weight
        }
    in

    { t
    ; stats = { stack = NonStackable }
    }

let rotCorpses turns l =
    List.fold_left
        ( fun items i -> match i.t with
            | Corpse c when turns - c.turnDeceased > turnsCorpseRot -> items
            | _ -> i::items
        ) [] (List.rev l)

let canStack item itemOther =
    item.stats.stack <> NonStackable
    && itemOther.stats.stack <> NonStackable
    && item.t = itemOther.t

let equal item itemOther = item.t = itemOther.t
