module N = Notty
module A = N.A

module C = Common
module H = Hit

type attributes =
    | NoHands

type info =
    { symbol : string
    ; attributes : attributes list
    ; color : A.color
    ; name : string
    ; difficulty : int
    ; frequency : int
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
    [   { name = "newt"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.yellow
        ; difficulty = 1
        ; levelBase = 0
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 2
            ]
        ; speed = 6
        }
    ;   { name = "sewer rat"
        ; symbol = "r"
        ; attributes = [NoHands]
        ; color = C.brown
        ; difficulty = 1
        ; levelBase = 0
        ; frequency = 1
        ; hits =
            [ H.mkMelee Bite Physical 1 3
            ]
        ; speed = 12
        }
    ;   { name = "bat"
        ; symbol = "B"
        ; attributes = [NoHands]
        ; color = C.brown
        ; difficulty = 2
        ; levelBase = 0
        ; frequency = 1
        ; hits =
            [ H.mkMelee Bite Physical 1 4
            ]
        ; speed = 22
        }
    ;   { name = "brown mold"
        ; symbol = "F"
        ; attributes = [NoHands]
        ; color = C.brown
        ; difficulty = 2
        ; levelBase = 2
        ; frequency = 1
        ; hits =
            [ H.mkPassive Cold 6
            ]
        ; speed = 0
        }
    ;   { name = "red mold"
        ; symbol = "F"
        ; attributes = [NoHands]
        ; color = A.red
        ; difficulty = 2
        ; levelBase = 2
        ; frequency = 1
        ; hits =
            [ H.mkPassive Fire 6
            ]
        ; speed = 0
        }
    ;   { name = "giant bat"
        ; symbol = "B"
        ; attributes = [NoHands]
        ; color = A.red
        ; difficulty = 3
        ; levelBase = 2
        ; frequency = 2
        ; hits =
            [ H.mkMelee Bite Physical 1 6
            ]
        ; speed = 22
        }
    ;   { name = "iguana"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.yellow
        ; difficulty = 3
        ; levelBase = 2
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 4
            ]
        ; speed = 6
        }
    ;   { name = "hill orc"
        ; symbol = "o"
        ; attributes = []
        ; color = A.lightyellow
        ; difficulty = 4
        ; levelBase = 2
        ; frequency = 2
        ; hits =
            [ H.mkWeapon 1 6
            ]
        ; speed = 9
        }
    ;   { name = "rothe"
        ; symbol = "q"
        ; attributes = [NoHands]
        ; color = C.brown
        ; difficulty = 4
        ; levelBase = 2
        ; frequency = 4
        ; hits =
            [ H.mkMelee Claw Physical 1 3
            ; H.mkMelee Bite Physical 1 3
            ; H.mkMelee Bite Physical 1 8
            ]
        ; speed = 9
        }
    ;   { name = "Uruk-hai"
        ; symbol = "o"
        ; attributes = []
        ; color = A.black
        ; difficulty = 5
        ; levelBase = 3
        ; frequency = 1
        ; hits =
            [ H.mkWeapon 1 8
            ]
        ; speed = 7
        }
    ;   { name = "human zombie"
        ; symbol = "Z"
        ; attributes = []
        ; color = A.lightwhite
        ; difficulty = 5
        ; levelBase = 4
        ; frequency = 1
        ; hits =
            [ H.mkMelee Claw Physical 1 8
            ]
        ; speed = 6
        }
    ;   { name = "giant beetle"
        ; symbol = "a"
        ; attributes = [NoHands]
        ; color = A.black
        ; difficulty = 6
        ; levelBase = 5
        ; frequency = 3
        ; hits =
            [ H.mkMelee Bite Physical 3 6
            ]
        ; speed = 6
        }
    ;   { name = "quivering blob"
        ; symbol = "b"
        ; attributes = [NoHands]
        ; color = A.lightwhite
        ; difficulty = 6
        ; levelBase = 5
        ; frequency = 2
        ; hits =
            [ H.mkMelee Touch Physical 1 8
            ]
        ; speed = 1
        }
    ;   { name = "lizard"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.green
        ; difficulty = 6
        ; levelBase = 5
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 6
            ]
        ; speed = 6
        }
    ;   { name = "mumak"
        ; symbol = "q"
        ; attributes = [NoHands]
        ; color = A.lightblack
        ; difficulty = 7
        ; levelBase = 5
        ; frequency = 1
        ; hits =
            [ H.mkMelee Butt Physical 4 12
            ; H.mkMelee Bite Physical 2 6
            ]
        ; speed = 9
        }
    ;   { name = "yeti"
        ; symbol = "Y"
        ; attributes = []
        ; color = A.lightwhite
        ; difficulty = 7
        ; levelBase = 5
        ; frequency = 2
        ; hits =
            [ H.mkMelee Claw Physical 1 6
            ; H.mkMelee Claw Physical 1 6
            ; H.mkMelee Bite Physical 1 4
            ]
        ; speed = 15
        }
    ;   { name = "gargoyle"
        ; symbol = "g"
        ; attributes = []
        ; color = C.brown
        ; difficulty = 8
        ; levelBase = 6
        ; frequency = 2
        ; hits =
            [ H.mkMelee Claw Physical 2 6
            ; H.mkMelee Claw Physical 2 6
            ; H.mkMelee Bite Physical 2 4
            ]
        ; speed = 10
        }
    ;   { name = "warhorse"
        ; symbol = "u"
        ; attributes = [NoHands]
        ; color = C.brown
        ; difficulty = 9
        ; levelBase = 7
        ; frequency = 2
        ; hits =
            [ H.mkMelee Kick Physical 1 10
            ; H.mkMelee Bite Physical 1 4
            ]
        ; speed = 24
        }
    ;   { name = "winged gargoyle"
        ; symbol = "g"
        ; attributes = []
        ; color = A.magenta
        ; difficulty = 11
        ; levelBase = 9
        ; frequency = 1
        ; hits =
            [ H.mkMelee Claw Physical 3 6
            ; H.mkMelee Claw Physical 3 6
            ; H.mkMelee Bite Physical 3 4
            ]
        ; speed = 15
        }
    ;   { name = "minotaur"
        ; symbol = "H"
        ; attributes = []
        ; color = C.brown
        ; difficulty = 17
        ; levelBase = 15
        ; frequency = 1
        ; hits =
            [ H.mkMelee Claw Physical 3 10
            ; H.mkMelee Claw Physical 3 10
            ; H.mkMelee Butt Physical 2 8
            ]
        ; speed = 15
        }
    ;   { name = "red dragon"
        ; symbol = "D"
        ; attributes = [NoHands]
        ; color = A.lightred
        ; difficulty = 20
        ; levelBase = 15
        ; frequency = 1
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
    ; inventory = if hasAttackWeapon ci && C.oneIn 2 then [Item.rnWeapon ()] else []
    ; info = ci
    }

let random difficultyLevel =
    let difficultyMin = difficultyLevel / 6 + 1 in

    let creaturesOk = List.filter (fun c -> c.difficulty >= difficultyMin && c.difficulty <= difficultyLevel) creatures in
    if List.is_empty creaturesOk then None else

    let freq = List.map (fun c -> c, c.frequency) creaturesOk in

    let creatureInfo = C.rnRelative freq in
    Some (mkCreature creatureInfo)

let getAttacksPassive c =
    c.info.hits
    |> List.filter (Hit.isPassive)
    |> List.map (Hit.toPassive)


let getWeaponForThrow c = match Item.getWeaponsByDamage c.inventory with
    | [] | _::[] -> None
    | _::sd::_ -> Some sd

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

let hasAttribute c a = List.mem a c.info.attributes

let canOpenDoor c = not (hasAttribute c NoHands)

let xpOnKill c = (c.level * c.level) + 1
