module N = Notty
module A = N.A

module R = Random_

type effect_t =
    | Cold
    | Dig
    | Electric
    | Fire
    | Paralyze
    | Physical
    | Sonic

type passive =
    { maxRoll : int
    ; effect_t : effect_t
    }

type melee_t =
    | Bite
    | Butt
    | Claw
    | Kick
    | Touch

type ranged_t =
    | Breath

type stats =
    { roll : R.roll
    ; effect_t : effect_t
    }

type ranged =
    { ranged_t : ranged_t
    ; stats : stats
    }

type melee =
    { melee_t : melee_t
    ; stats : stats
    }

type t =
    | Passive of passive
    | Ranged of ranged
    | Melee of melee
    | Weapon of R.roll

type msgs =
    { msgHit : string
    ; msgCause : string
    ; msgEffect : string
    }

let mkPassive e maxRoll = Passive
    { effect_t = e
    ; maxRoll
    }

let mkMelee t e rolls sides = Melee
    { melee_t = t
    ; stats =
        { effect_t = e
        ; roll = { rolls; sides }
        }
    }

let mkRanged t e rolls sides = Ranged
    { ranged_t = t
    ; stats =
        { effect_t = e
        ; roll = { rolls; sides }
        }
    }

let mkWeapon rolls sides = Weapon { rolls; sides }

let getEffect = function
    | Passive p -> p.effect_t
    | Ranged r -> r.stats.effect_t
    | Melee m -> m.stats.effect_t
    | Weapon _ -> Physical

let getMsgsEffect e =
    let msgCause, msgEffect = match e with
        | Cold -> "cold", "freezes"
        | Dig -> "digging spell", "passes through"
        | Electric -> "electricity", "zaps"
        | Fire -> "fire", "burns"
        | Paralyze -> "paralysis", "stiffens"
        | Physical -> "attack", "hits"
        | Sonic -> "sound blast", "rattles"
    in
    { msgHit = ""; msgCause; msgEffect }

let isPassive = function
    | Passive _ -> true
    | _ -> false

let toPassive = function
    | Passive p -> p
    | _ -> assert false

let isRanged = function
    | Ranged _ -> true
    | _ -> false

let toRanged = function
    | Ranged p -> p
    | _ -> assert false

let getMsgs a =
    let msgHit = match a with
        | Passive _ -> "jiggles"
        | Ranged r ->
            ( match r.ranged_t with
                | Breath -> "breathes"
            )

        | Melee m ->
            ( match m.melee_t with
            | Bite -> "bites"
            | Butt -> "butts"
            | Claw -> "claws at"
            | Kick -> "kicks"
            | Touch -> "touches"
            )

        | Weapon _ -> "attacks"
    in
    let msgBase = getMsgsEffect (getEffect a) in
    { msgBase with msgHit }

let getImageForAnimation t (dir : Position.dir) =
    let mColor = match t with
    | Cold -> Some A.(fg cyan)
    | Dig ->  None
    | Electric -> Some A.(fg lightyellow)
    | Fire -> Some A.(fg lightred)
    | Paralyze -> None
    | Physical -> Some A.(fg white)
    | Sonic -> None
    in

    let c = match dir with
    | _ when dir.dr = 0 -> "-"
    | _ when dir.dc = 0 -> "|"
    | _ when dir.dr = dir.dc -> "\\"
    | _ when dir.dr <> dir.dc -> "/"
    | _ -> assert false
    in

    Option.map (fun color -> N.I.string A.(Config.styleBg ++ st bold ++ color) c) mColor
