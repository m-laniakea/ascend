module N = Notty
module A = N.A

module C = Common
module H = Hit

type info =
    { symbol : string
    ; color : A.color
    ; name : string
    ; difficulty : int
    ; levelBase : int
    ; hits : Hit.t list
    ; speed : int
    }

type t =
    { hp : int
    ; level : int
    ; pointsSpeed : int
    ; info : info
    }

let creatures =
    [ { name = "red dragon"
        ; symbol = "D"
        ; color = A.lightred
        ; difficulty = 20
        ; levelBase = 15
        ; hits =
            [ H.mkRanged Breath Fire 6 6
            ; H.mkMelee Bite Physical 3 8
            ; H.mkMelee Claw Physical 1 4
            ; H.mkMelee Claw Physical 1 4
            ]
        ; speed = 9
        }
    ]

let mkCreature ci =
    { hp = 7
    ; level = 7
    ; pointsSpeed = ci.speed
    ; info = ci
    }

let random () =
    let creatureInfo = C.rnItem creatures in
    mkCreature creatureInfo

let hasRangedAttack c =
    c.info.hits
    |> List.exists (function | Hit.Ranged _ -> true | _ -> false)

let hasTurn c = match c.pointsSpeed with
    | ps when ps <= 0 -> false
    | ps when ps >= C.pointsSpeedPerTurn -> true
    | ps -> C.rn 1 C.pointsSpeedPerTurn <= ps
