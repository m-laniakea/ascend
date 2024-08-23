open Matrix

module C = Common
module Cr = Creature
module S = State
module SL = StateLevels

let addHp ~sourceIsPlayer n t p (c : Creature.t) state =
    let cl = SL.map state in
    let state, t' =
        if c.hp + n < 0 then
            (* TODO drops *)
            let _ = if Sight.playerCanSee state p then S.msgAdd state (C.sf "The %s is killed!" c.info.name) else () in
            let deathDrops =
                let corpse = Item.mkCorpse c.info.name c.info.color c.info.weight state.turns in
                (* TODO Not every creature can leave a corpse *)
                let item = if Random_.oneIn 6 then [Item.random ()] else [] in
                corpse::item @ c.inventory
            in
            let state = if sourceIsPlayer then UpdatePlayer.xpAdd (Cr.xpOnKill c) state else state in
            state, Map.{ t with occupant = None; items = deathDrops @ t.items }
        else
            let c' = Map.Creature { c with hp = c.hp + n } in
            state, { t with occupant = Some c' }
    in
    let cl' = Matrix.set t' p cl in
    SL.setMap cl' state
