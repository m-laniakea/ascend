open Matrix

module C = Common
module Cr = Creature
module S = State
module SL = StateLevels

let addHp ~sourceIsPlayer n p (c : Creature.t) state =
    let m = SL.map state in
    let t = Matrix.get m p in
    let state, t' =
        if c.hp + n < 0 then
            let isPet = Creature.isPet c in
            let whos = if isPet then "Your" else "The" in
            let _ = if Sight.playerCanSee state p then
                    S.msgAdd state (C.sf "%s %s is killed!" whos c.info.name)
                else if Cr.isPet c then
                    S.msgAdd state "You have a sad feeling for a moment."
            in
            let deathDrops =
                let corpse = Item.mkCorpse c.info.name c.info.color c.info.weight state.turns in
                (* TODO Not every creature can leave a corpse *)
                let item = if isPet then [] else if Random_.oneIn 6 then [Item.random ()] else [] in
                corpse::item @ c.inventory
            in
            let state = if sourceIsPlayer then UpdatePlayer.xpAdd (Cr.xpOnKill c) state else state in
            state, Map.{ t with occupant = None; items = deathDrops @ t.items }
        else
            let c' = Map.Creature { c with hp = c.hp + n } in
            state, { t with occupant = Some c' }
    in
    let m' = Matrix.set t' p m in
    SL.setMap m' state
