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
    ; inventory : Item.t list
    ; info : info
    }

let creatures =
    [   { name = "sewer rat"
        ; symbol = "r"
        ; color = C.brown
        ; difficulty = 1
        ; levelBase = 0
        ; hits =
            [ H.mkMelee Bite Physical 1 3
            ]
        ; speed = 12
        }
    ;   { name = "giant bat"
        ; symbol = "B"
        ; color = A.red
        ; difficulty = 3
        ; levelBase = 2
        ; hits =
            [ H.mkMelee Bite Physical 1 6
            ]
        ; speed = 22
        }
    ;   { name = "hill orc"
        ; symbol = "o"
        ; color = A.lightyellow
        ; difficulty = 4
        ; levelBase = 2
        ; hits =
            [ H.mkWeapon 1 6
            ]
        ; speed = 9
        }
    ;   { name = "Uruk-hai"
        ; symbol = "o"
        ; color = A.black
        ; difficulty = 5
        ; levelBase = 3
        ; hits =
            [ H.mkWeapon 1 8
            ]
        ; speed = 7
        }
    ;   { name = "human zombie"
        ; symbol = "Z"
        ; color = A.lightwhite
        ; difficulty = 5
        ; levelBase = 4
        ; hits =
            [ H.mkMelee Claw Physical 1 8
            ]
        ; speed = 6
        }
    ;   { name = "mumak"
        ; symbol = "q"
        ; color = A.lightblack
        ; difficulty = 7
        ; levelBase = 5
        ; hits =
            [ H.mkMelee Butt Physical 4 12
            ; H.mkMelee Bite Physical 2 6
            ]
        ; speed = 9
        }
    ;   { name = "warhorse"
        ; symbol = "u"
        ; color = C.brown
        ; difficulty = 9
        ; levelBase = 7
        ; hits =
            [ H.mkMelee Kick Physical 1 10
            ; H.mkMelee Bite Physical 1 4
            ]
        ; speed = 24
        }
    ;   { name = "winged gargoyle"
        ; symbol = "g"
        ; color = A.magenta
        ; difficulty = 11
        ; levelBase = 9
        ; hits =
            [ H.mkMelee Claw Physical 3 6
            ; H.mkMelee Claw Physical 3 6
            ; H.mkMelee Bite Physical 3 4
            ]
        ; speed = 15
        }
    ;   { name = "minotaur"
        ; symbol = "H"
        ; color = C.brown
        ; difficulty = 17
        ; levelBase = 15
        ; hits =
            [ H.mkMelee Claw Physical 3 10
            ; H.mkMelee Claw Physical 3 10
            ; H.mkMelee Butt Physical 2 8
            ]
        ; speed = 15
        }
    ;   { name = "red dragon"
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

let rollHp ci = match ci.levelBase with
    | l when l < 0 -> assert false
    | 0 -> C.rn 1 4
    | l -> C.doRoll { rolls = l; sides = 8 }

let hasAttackWeapon ci = List.exists (function | Hit.Weapon _ -> true | _ -> false) ci.hits

let mkCreature ci =
    { hp = rollHp ci
    ; level = ci.levelBase
    ; pointsSpeed = ci.speed
    ; inventory = if hasAttackWeapon ci then [Item.rnWeapon ()] else []
    ; info = ci
    }

let random difficultyLevel =
    let difficultyMin = difficultyLevel / 6 + 1 in

    let creaturesOk = List.filter (fun c -> c.difficulty >= difficultyMin && c.difficulty <= difficultyLevel) creatures in
    if List.is_empty creaturesOk then None else

    let creatureInfo = C.rnItem creaturesOk in
    Some (mkCreature creatureInfo)

let getWeaponForThrow c = match Item.getWeaponsByDamage c.inventory with
    | [] | _::[] -> None
    | md::sd::_ -> Some sd

let hasAttackRanged c =
    c.info.hits
    |> List.exists
        ( function
            | Hit.Ranged _ -> true
            | Hit.Weapon _ -> getWeaponForThrow c |> Option.is_some
            | _ -> false
        )

let hasTurn c = match c.pointsSpeed with
    | ps when ps <= 0 -> false
    | ps when ps >= C.pointsSpeedPerTurn -> true
    | ps -> C.rn 1 C.pointsSpeedPerTurn <= ps
