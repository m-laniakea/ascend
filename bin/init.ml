module P = Position
module S = State
module SL = StateLevels

let intro =
    [ "\"Welcome\" to the Dungeons."
    ; ""
    ; "In the beginning of the words, there was Rievax the Revelator."
    ; "A long time ago, Gnilsog the Corruptor deceived the world and stole from the Revelator an artifact of immense power:"
    ; "The Scepter of Yorel."
    ; ""
    ; "Far below, in the furthest depths of the dungeon, Gnilsog now takes joy in watching the continuing stream of"
    ; "prisoners perish in infinite ways. All the while, the Corrupter continues to abuse the might of the Scepter"
    ; "to maintain immortality, to rule unjustly, and to benight the inhabitants of the overworld."
    ; ""
    ; "Having been captured and imprisoned by the forces of Gnilsog,"
    ; "You, secret and loyal follower of Rievax the Revelator,"
    ; "know that you must retrieve the Scepter of Yorel from the depths..."
    ; "or die in the attempt."
    ]

let state =
    Random.init 662;

    let levels =
        S.
        { indexLevel = -1
        ; levels = []
        ; hasBigroom = false
        ; hasGarden = false
        }
    in

    let player =
        S.
        { attributes = []
        ; pos = { row = 0; col = 0 }
        ; gold = 20
        ; hp = 10
        ; hpMax = 10
        ; level = 1
        ; xp = 0
        ; status = []
        ; acBonus = 0
        ; weaponWielded = None
        ; inventory = []
        ; inventoryWeightMax = 500
        ; knowledgeLevels = []
        ; timesKilled = 0
        ; turnHealthWarned = 0
        }
    in

    let stateI =
        S.
        { levels
        ; player
        ; endgame = BeforeEndgame
        ; messages = Queue.create ()
        ; mode = DisplayText intro
        ; turns = 0
        }
    in
    Queue.push "Welcome!" stateI.messages;
    Queue.push "There is a moon tonight." stateI.messages;
    Queue.push "" stateI.messages;
    Queue.push "Hint: (s)earch" stateI.messages;
    GenMap.gen stateI
    |> Player.moveToStairs ~dir:Up
    |> UpdatePlayer.knowledgeMapAddEmpty
    |> UpdatePlayer.knowledgeMap
