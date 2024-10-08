module L = List

module N = Notty
module A = N.A

module C = Common
module H = Hit
module M = Matrix.Matrix
module P = Position
module R = Random_

let rangeThrown = 8
let rangeRangedMin = 20
let rangeRangedMax = 26
let range2RangedMax = rangeRangedMax * rangeRangedMax

type hostility =
    | Docile
    | Hostile
    | Peaceful
    | Tame

type sizeGroupSpawn =
    | GroupSmall
    | GroupLarge

type attributes =
    | Blind
    | Domestic
    | FollowAlways
    | FollowStairs
    | MoveGrid
    | NoHands
    | Mindless
    | Resist of Hit.effect_t
    | SpawnGroup of sizeGroupSpawn
    | Telepathic
    | Vulnerable of Hit.effect_t

type info =
    { acBase : int
    ; attributes : attributes list
    ; color : A.color
    ; difficulty : int
    ; frequency : int
    ; hits : Hit.t list
    ; levelBase : int
    ; name : string
    ; speed : int
    ; symbol : string
    ; weight : int
    }

type t =
    { id : R.uid
    ; hp : int
    ; hpMax : int
    ; hostility : hostility
    ; info : info
    ; inventory : Item.t list
    ; level : int
    ; pointsSpeed : int
    }

let idGnilsog = R.uid ()

let infoDragon =
    { name = "red dragon"
    ; symbol = "D"
    ; attributes =
        [ NoHands
        ; Resist Fire
        ]
    ; color = A.lightred
    ; difficulty = 20
    ; levelBase = 15
    ; acBase = -1
    ; frequency = 1
    ; hits =
        [ H.mkRanged Breath Fire 6 6
        ; H.mkMelee Bite Physical 3 8
        ; H.mkMelee Claw Physical 1 4
        ; H.mkMelee Claw Physical 1 4
        ]
    ; speed = 9
    ; weight = 4500
    }

let creatures =
    [   { name = "newt"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.yellow
        ; difficulty = 1
        ; levelBase = 0
        ; acBase = 8
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 2
            ]
        ; speed = 6
        ; weight = 10
        }
    ;   { name = "grid bug"
        ; symbol = "x"
        ; attributes =
            [ MoveGrid
            ; NoHands
            ]
        ; color = A.magenta
        ; difficulty = 1
        ; levelBase = 0
        ; acBase = 9
        ; frequency = 3
        ; hits =
            [ H.mkMelee Bite Electric 1 1
            ]
        ; speed = 12
        ; weight = 15
        }
    ;   { name = "sewer rat"
        ; symbol = "r"
        ; attributes =
            [ Domestic
            ; SpawnGroup GroupSmall
            ; NoHands
            ]
        ; color = C.brown
        ; difficulty = 1
        ; levelBase = 0
        ; acBase = 7
        ; frequency = 1
        ; hits =
            [ H.mkMelee Bite Physical 1 3
            ]
        ; speed = 12
        ; weight = 20
        }
    ;   { name = "bat"
        ; symbol = "B"
        ; attributes =
            [ SpawnGroup GroupSmall
            ; NoHands
            ]
        ; color = C.brown
        ; difficulty = 2
        ; levelBase = 0
        ; acBase = 8
        ; frequency = 1
        ; hits =
            [ H.mkMelee Bite Physical 1 4
            ]
        ; speed = 22
        ; weight = 20
        }
    ;   { name = "brown mold"
        ; symbol = "F"
        ; attributes =
            [ NoHands
            ; Mindless
            ]
        ; color = C.brown
        ; difficulty = 2
        ; levelBase = 2
        ; acBase = 9
        ; frequency = 1
        ; hits =
            [ H.mkPassive Cold 6
            ]
        ; speed = 0
        ; weight = 50
        }
    ;   { name = "red mold"
        ; symbol = "F"
        ; attributes =
            [ NoHands
            ; Mindless
            ]
        ; color = A.red
        ; difficulty = 2
        ; levelBase = 2
        ; acBase = 9
        ; frequency = 1
        ; hits =
            [ H.mkPassive Fire 6
            ]
        ; speed = 0
        ; weight = 50
        }
    ;   { name = "floating eye"
        ; symbol = "e"
        ; attributes = [NoHands]
        ; color = A.lightblue
        ; difficulty = 3
        ; levelBase = 2
        ; acBase = 9
        ; frequency = 2
        ; hits =
            [ H.mkPassive Paralyze 25
            ]
        ; speed = 1
        ; weight = 10
        }
    ;   { name = "giant bat"
        ; symbol = "B"
        ; attributes = [NoHands]
        ; color = A.red
        ; difficulty = 3
        ; levelBase = 2
        ; acBase = 7
        ; frequency = 2
        ; hits =
            [ H.mkMelee Bite Physical 1 6
            ]
        ; speed = 22
        ; weight = 30
        }
    ;   { name = "iguana"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.yellow
        ; difficulty = 3
        ; levelBase = 2
        ; acBase = 7
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 4
            ]
        ; speed = 6
        ; weight = 30
        }
    ;   { name = "hill orc"
        ; symbol = "o"
        ; attributes =
            [ SpawnGroup GroupLarge
            ]
        ; color = A.lightyellow
        ; difficulty = 4
        ; levelBase = 2
        ; acBase = 10
        ; frequency = 2
        ; hits =
            [ H.mkWeapon 1 6
            ]
        ; speed = 9
        ; weight = 1000
        }
    ;   { name = "rothe"
        ; symbol = "q"
        ; attributes =
            [ SpawnGroup GroupSmall
            ; NoHands
            ]
        ; color = C.brown
        ; difficulty = 4
        ; levelBase = 2
        ; acBase = 7
        ; frequency = 4
        ; hits =
            [ H.mkMelee Claw Physical 1 3
            ; H.mkMelee Bite Physical 1 3
            ; H.mkMelee Bite Physical 1 8
            ]
        ; speed = 9
        ; weight = 400
        }
    ;   { name = "Uruk-hai"
        ; symbol = "o"
        ; attributes =
            [ SpawnGroup GroupLarge
            ]
        ; color = A.black
        ; difficulty = 5
        ; levelBase = 3
        ; acBase = 10
        ; frequency = 1
        ; hits =
            [ H.mkWeapon 1 8
            ]
        ; speed = 7
        ; weight = 1300
        }
    ;   { name = "human zombie"
        ; symbol = "Z"
        ; attributes =
            [ FollowStairs
            ; Mindless
            ; SpawnGroup GroupSmall
            ]
        ; color = A.lightwhite
        ; difficulty = 5
        ; levelBase = 4
        ; acBase = 8
        ; frequency = 1
        ; hits =
            [ H.mkMelee Claw Physical 1 8
            ]
        ; speed = 6
        ; weight = 1450
        }
    ;   { name = "giant beetle"
        ; symbol = "a"
        ; attributes = [NoHands]
        ; color = A.black
        ; difficulty = 6
        ; levelBase = 5
        ; acBase = 4
        ; frequency = 3
        ; hits =
            [ H.mkMelee Bite Physical 3 6
            ]
        ; speed = 6
        ; weight = 200
        }
    ;   { name = "quivering blob"
        ; symbol = "b"
        ; attributes =
            [ Mindless
            ; NoHands
            ]
        ; color = A.lightwhite
        ; difficulty = 6
        ; levelBase = 5
        ; acBase = 8
        ; frequency = 2
        ; hits =
            [ H.mkMelee Touch Physical 1 8
            ]
        ; speed = 1
        ; weight = 200
        }
    ;   { name = "lizard"
        ; symbol = ":"
        ; attributes = [NoHands]
        ; color = A.green
        ; difficulty = 6
        ; levelBase = 5
        ; acBase = 6
        ; frequency = 5
        ; hits =
            [ H.mkMelee Bite Physical 1 6
            ]
        ; speed = 6
        ; weight = 10
        }
    ;   { name = "call duck"
        ; symbol = "p"
        ; attributes =
            [ Domestic
            ; NoHands
            ]
        ; color = A.gray 13
        ; difficulty = 7
        ; levelBase = 6
        ; acBase = 4
        ; frequency = 1
        ; hits =
            [ H.mkMelee Bite Physical 2 4
            ]
        ; speed = 15
        ; weight = 250
        }
    ;   { name = "mumak"
        ; symbol = "q"
        ; attributes = [NoHands]
        ; color = A.lightblack
        ; difficulty = 7
        ; levelBase = 5
        ; acBase = 0
        ; frequency = 1
        ; hits =
            [ H.mkMelee Butt Physical 4 12
            ; H.mkMelee Bite Physical 2 6
            ]
        ; speed = 9
        ; weight = 2500
        }
    ;   { name = "yeti"
        ; symbol = "Y"
        ; attributes = []
        ; color = A.lightwhite
        ; difficulty = 7
        ; levelBase = 5
        ; acBase = 6
        ; frequency = 2
        ; hits =
            [ H.mkMelee Claw Physical 1 6
            ; H.mkMelee Claw Physical 1 6
            ; H.mkMelee Bite Physical 1 4
            ]
        ; speed = 15
        ; weight = 1600
        }
    ;   { name = "gargoyle"
        ; symbol = "g"
        ; attributes =
            [ Vulnerable Sonic
            ]
        ; color = C.brown
        ; difficulty = 8
        ; levelBase = 6
        ; acBase = -4
        ; frequency = 2
        ; hits =
            [ H.mkMelee Claw Physical 2 6
            ; H.mkMelee Claw Physical 2 6
            ; H.mkMelee Bite Physical 2 4
            ]
        ; speed = 10
        ; weight = 1000
        }
    ;   { name = "warhorse"
        ; symbol = "u"
        ; attributes =
            [ Domestic
            ; NoHands
            ]
        ; color = C.brown
        ; difficulty = 9
        ; levelBase = 7
        ; acBase = 4
        ; frequency = 1
        ; hits =
            [ H.mkMelee Kick Physical 1 10
            ; H.mkMelee Bite Physical 1 4
            ]
        ; speed = 24
        ; weight = 1800
        }
    ;   { name = "winged gargoyle"
        ; symbol = "g"
        ; attributes =
            [ Vulnerable Sonic
            ]
        ; color = A.magenta
        ; difficulty = 11
        ; levelBase = 9
        ; acBase = -2
        ; frequency = 1
        ; hits =
            [ H.mkMelee Claw Physical 3 6
            ; H.mkMelee Claw Physical 3 6
            ; H.mkMelee Bite Physical 3 4
            ]
        ; speed = 15
        ; weight = 1200
        }
    ; infoDragon
    ]

let rollHp ci = match ci.levelBase with
    | l when l < 0 -> assert false
    | 0 -> R.rn 1 4
    | l -> R.roll { rolls = l; sides = 8 }

let hasAttackWeapon ci = List.exists (function | Hit.Weapon _ -> true | _ -> false) ci.hits

let mkCreature ?(level=None) ci =
    let hpMax = rollHp ci in
    { id = R.uid ()
    ; hp = hpMax
    ; hpMax
    ; hostility = Hostile
    ; level = Option.value level ~default:ci.levelBase
    ; pointsSpeed = ci.speed
    ; inventory = if hasAttackWeapon ci && R.oneIn 2 then [Item.rnWeapon ()] else []
    ; info = ci
    }

let mkCreatures ?(level=None) ci n =
    L.init n (fun _ -> mkCreature ~level ci)


let mkGnilsog timesKilled =
    let info =
        { acBase = 0
        ; attributes = []
        ; color = A.magenta
        ; difficulty = 21
        ; frequency = 0
        ; hits =
            [ H.mkMelee Claw Physical 2 6 (* TODO steal scepter *)
            ]
        ; levelBase = 17
        ; name = "Gnilsog"
        ; speed = 12
        ; symbol = "@"
        ; weight = 1450
        }
    in

    let levelBase = info.levelBase + timesKilled in
    let difficulty = info.difficulty + timesKilled in
    let attributes = if timesKilled <= 0 then info.attributes else FollowAlways::info.attributes in
    let info =
        { info with levelBase
        ; attributes
        ; difficulty
        }
    in
    let gnilsog = mkCreature info in
    let id = idGnilsog in
    { gnilsog with id }

let mkAurochs () =
    let info =
        { acBase = 0
        ; attributes = [NoHands]
        ; color = A.lightwhite
        ; difficulty = 7
        ; frequency = 0
        ; hits =
            [ H.mkMelee Butt Physical 4 12
            ]
        ; levelBase = 5
        ; name = "aurochs"
        ; speed = 9
        ; symbol = "U"
        ; weight = 2500
        }
    in
    { (mkCreature info) with hostility = Peaceful
    }

let mkButterfly () =
    let info =
        { acBase = 0
        ; attributes = [NoHands]
        ; color = R.item A.[ (gray 8); blue; lightblue; lightblue; lightcyan; lightyellow; lightred ]
        ; difficulty = 0
        ; frequency = 0
        ; hits =
            [ H.mkMelee Touch Physical 0 1
            ]
        ; levelBase = 0
        ; name = "butterfly"
        ; speed = 18
        ; symbol = "B"
        ; weight = 1
        }
    in
    { (mkCreature info) with hostility = Docile
    }

let infoMitras =
    { acBase = -10
    ; attributes =
        [ Telepathic
        ]
    ; color = A.white
    ; difficulty = 26
    ; frequency = 0
    ; hits =
        [ H.mkWeapon 2 6
        ; H.mkWeapon 2 6
        ; H.mkMelee Touch Fire 5 62
        ]
    ; levelBase = 26
    ; name = "Mitras"
    ; speed = 26
    ; symbol = "A"
    ; weight = 1450
    }

let mkMitras () =
    { (mkCreature infoMitras) with hostility = Peaceful
    ; inventory = []
    }

let mkMinotaur () =
    let info =
        { name = "minotaur"
        ; symbol = "H"
        ; attributes = []
        ; color = C.brown
        ; difficulty = 17
        ; levelBase = 15
        ; acBase = 6
        ; frequency = 0
        ; hits =
            [ H.mkMelee Claw Physical 3 10
            ; H.mkMelee Claw Physical 3 10
            ; H.mkMelee Butt Physical 2 8
            ]
        ; speed = 15
        ; weight = 1500
        }
    in
    let wand =
        Item.
        { t = Wand { wand_t = Dig; charges = 13 }
        ; stats = { stack = NonStackable }
        }
    in
    let inventory = [ wand ] in
    { (mkCreature info) with inventory
    }

let mkCaptain () =
    let info =
        { acBase = 0
        ; attributes =
            [ FollowStairs
            ]
        ; color = A.blue
        ; difficulty = 14
        ; frequency = 0
        ; hits =
            [ H.mkWeapon 4 4
            ; H.mkWeapon 4 4
            ]
        ; levelBase = 12
        ; name = "captain"
        ; speed = 10
        ; symbol = "@"
        ; weight = 1450
        }
    in
    mkCreature info

let mkDragon ~telepathic =
    let attr = infoDragon.attributes in
    let attributes = if telepathic then Telepathic::attr else attr in
    let info = { infoDragon with attributes } in
    mkCreature info

let getLevel difficulty levelBase =
    let bonusDepth = (difficulty - levelBase |> max 0) / 4 in
    let levelMax = levelBase * 3 / 2 in
    let level = levelBase + bonusDepth in
    min level levelMax

let random difficultyLevel =
    let difficultyMin = difficultyLevel / 6 + 1 in

    let creaturesOk = List.filter (fun c -> c.difficulty >= difficultyMin && c.difficulty <= difficultyLevel) creatures in
    if List.is_empty creaturesOk then [] else

    let freq = List.map (fun c -> c, c.frequency) creaturesOk in
    let ci = R.relative freq in
    let level = Some (getLevel difficultyLevel ci.levelBase) in

    let attrSpawn = L.filter_map (function | SpawnGroup s -> Some s | _ -> None) ci.attributes in
    let count = match attrSpawn with
        | [] -> 1
        | GroupSmall::_ ->
            if R.oneIn 2 then
                R.rn 2 4 |> min difficultyLevel
            else
                1
        | GroupLarge::_ -> R.rn 2 4 + R.rn 0 4
    in
    mkCreatures ~level ci count

let getAttacksPassive c =
    c.info.hits
    |> List.filter (Hit.isPassive)
    |> List.map (Hit.toPassive)

let getAttacksRanged c =
    c.info.hits
    |> List.filter (Hit.isRanged)
    |> List.map (Hit.toRanged)

let getWeaponForThrow c = match Item.getWeaponsByDamage c.inventory with
    | [] | _::[] -> None
    | _::sd::_ -> Some sd

let hasAttackRangedWeapon c =
    c.info.hits
    |> List.exists
        ( function
            | Hit.Weapon _ -> getWeaponForThrow c |> Option.is_some
            | _ -> false
        )

let hasAttackRanged c =
    c.info.hits
    |> List.exists
        ( function
            | Hit.Ranged _ -> true
            | _ -> false
        )

let hasTurn = function
    | ps when ps <= 0 -> false
    | ps when ps >= C.pointsSpeedPerTurn -> true
    | ps -> R.rn 1 C.pointsSpeedPerTurn <= ps

let hasAttribute c a = List.mem a c.info.attributes

let isBlind = List.exists (function | Blind -> true | _ -> false)
let isDocile c = c.hostility = Docile
let isHostile c = c.hostility = Hostile
let isPeaceful c = c.hostility = Peaceful
let isMindless = List.exists (function | Mindless -> true | _ -> false)
let isPet c = c.hostility = Tame
let isTelepath = List.exists (function | Telepathic -> true | _ -> false)

let isTameable c = hasAttribute c Domestic

let isFollowerStairs c = c.hostility = Tame || hasAttribute c FollowStairs
let isFollowerAlways c = hasAttribute c FollowAlways

let isResistant c e = hasAttribute c (Resist e)
let isVulnerable c e = hasAttribute c (Vulnerable e)

let canFollow pStairs c p =
    isFollowerAlways c
    || P.distance2 pStairs p <= 2
    && isFollowerStairs c

let canOpenDoor c = not (hasAttribute c NoHands)

let xpOnKill c = (c.level * c.level) + 1

let getAc c = c.info.acBase (* TODO current AC *)

let getFeeling levelObserver c =
    let scariness = (c.info.difficulty - (levelObserver / 2)) / 2 in
    let approachability = (levelObserver - c.info.difficulty) / 2 in

    match scariness with
    | _ when isPet c -> "confidence"
    | _ when not (isHostile c) -> "tranquility"

    | _ when approachability > 0 ->
        ( match approachability with
        | 1 -> "little concern"
        | _ -> "no concern"
        )
    | 0 -> "concern"
    | 1 -> "anxiety"
    | 2 -> "disquiet"
    | 3 -> "alarm"
    | _ -> "dread"
