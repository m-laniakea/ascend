module N = Notty
module A = N.A

module C = Common

type effect =
    | Physical
    | Fire

type passive =
    { maxRoll : int
    ; effect : effect
    }

type melee_t =
    | Bite
    | Claw

type ranged_t =
    | Breath

type stats =
    { roll : C.roll
    ; effect : effect
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

type msgs =
    { msgHit : string
    ; msgCause : string
    ; msgEffect : string
    }

let mkMelee t e rolls sides = Melee
    { melee_t = t
    ; stats =
        { effect = e
        ; roll = { rolls; sides }
        }
    }

let mkRanged t e rolls sides = Ranged
    { ranged_t = t
    ; stats =
        { effect = e
        ; roll = { rolls; sides }
        }
    }

let getEffect = function
    | Passive p -> p.effect
    | Ranged r -> r.stats.effect
    | Melee m -> m.stats.effect

let getMsgsCauseEffect a =
    let msgCause, msgEffect = match getEffect a with
        | Physical -> "attack", "hits"
        | Fire -> "fire", "burns"
    in
    { msgHit = ""; msgCause; msgEffect }

let getMsgs a =
    let msgHit = match a with
        | Passive p -> assert false (* TODO *)
        | Ranged r ->
            ( match r.ranged_t with
                | Breath -> "breathes"
            )

        | Melee m -> match m.melee_t with
            | Bite -> "bites"
            | Claw -> "claws at"
    in
    let msgBase = getMsgsCauseEffect a in
    { msgBase with msgHit }

let getImageForAnimation t dir =
    let color = match t with
    | Physical -> A.(fg white)
    | Fire -> A.(fg lightred)
    in

    let open Common in
    let c = match dir with
    | _ when dir.row = 0 -> "-"
    | _ when dir.col = 0 -> "|"
    | _ when dir.row = dir.col -> "\\"
    | _ when dir.row <> dir.col -> "/"
    | _ -> assert false
    in

    N.I.string A.(st bold ++ color) c
