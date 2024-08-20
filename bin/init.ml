module P = Position
module S = State
module SL = StateLevels

let stateInitial =
    Random.init 53;

    let stateLevels =
        S.
        { indexLevel = -1
        ; levels = []
        }
    in

    let statePlayer =
        S.
        { pos = { row = 0; col = 0 }
        ; gold = 20
        ; hp = 10
        ; hpMax = 10
        ; level = 1
        ; xp = 0
        ; acBonus = 0
        ; weaponWielded = None
        ; inventory = []
        ; knowledgeLevels = []
        ; turnHealthWarned = 0
        }
    in

    let stateI =
        S.
        { stateLevels
        ; statePlayer
        ; messages = Queue.create ()
        ; mode = Playing
        ; turns = 0
        }
    in
    Queue.push "Welcome!" stateI.messages;
    Queue.push "There is a moon tonight." stateI.messages;
    GenMap.gen stateI
    |> Player.moveToStairs ~dir:Up
    |> UpdatePlayer.knowledgeMapAddEmpty
    |> UpdatePlayer.knowledgeMap
