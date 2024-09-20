open Matrix

module C = Common
module Cr = Creature
module S = State
module SL = StateLevels

let onPeaceBroken state =
    let level = SL.level state in
    let m = SL.map state in

    match level.level_t with
    | Dungeon -> state (* TODO Attacking peaceful in dungeon *)
    | Final -> state (* TODO Attacking peaceful in final level *)
    | Garden idMitras -> match Map.getCreature idMitras m with
        | None -> state (* Mitras gone somehow... *)
        | Some (mitras, p) ->
            if mitras.hostility <> Peaceful then state else
            let attributes = Cr.FollowAlways::mitras.info.attributes in
            let info =
                { mitras.info with speed = Cr.infoMitras.speed
                ; attributes
                }
            in
            let mitras = { mitras with hostility = Hostile; info } in
            let occupant = Some (Map.Creature mitras) in
            let m = Map.setOccupant occupant p m in
            let _ = S.msgAdd state "Oh Dasyu, what hast thou done?" in
            let _ = S.msgAdd state "You sense a massive statue beginning to move..." in
            SL.setMap m state

let textGnilsogFirstKilled =
    [ "Gnilsog gasps, staggering backwards, before collapsing into a lifeless heap."
    ; ""
    ; "The vile Gnilsog lies slain before you!"
    ; ""
    ; "But before you can rejoice, the corpse's mouth opens wide, then freezes."
    ; "You hear: \"Didst thou truly believe thou couldst violate my sanctum,"
    ; "take my life, and abscond with my artifact unscathed, you THIEF?\""
    ; ""
    ; "You hear maniacal laughter in the distance..."
    ; "Gnilsog's cold, lifeless hand reaches into a mold-infested pocket,"
    ; "pulls out a small medallion and snaps it in two."
    ; ""
    ; "You feel the dungeon shudder."
    ; "Some mechanisms above the doors begins to whirr and clank..."
    ; "A petrifying shriek reverberates throughout the halls."
    ; "You sense metal rattling and groaning!"
    ; "Walls crumble, as a massive creature unleashes its rage..."
    ]

let getNextHarassment (state : S.t) = state.turns + Random_.rn 51 226

let onCreatureDeath (c : Creature.t) (state : S.t) =
    match c.id with
    | id when id = Cr.idGnilsog ->
        ( match state.endgame with
            | BeforeEndgame ->
                let endgame = S.Endgame
                    { timesGnilsogSlain = 1
                    ; gnilsogAlive = false
                    ; nextHarassment = getNextHarassment state
                    }
                in
                let mode = S.DisplayText textGnilsogFirstKilled in
                let state = UpdateMap.closeDoorsGnilsogIfAble state in
                let state = UpdateMap.lowerDragonGate state in
                { state with mode; endgame }

            | Endgame se ->
                let ts = se.timesGnilsogSlain in
                let endgame = S.Endgame
                    { timesGnilsogSlain = ts + 1
                    ; gnilsogAlive = false
                    ; nextHarassment = getNextHarassment state
                    }
                in
                { state with endgame }
        )
    | _ -> state

let addHp ~sourceIsPlayer n p (c : Creature.t) state =
    let state, t' =
        if c.hp + n <= 0 then
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
            let state = if sourceIsPlayer && c.hostility <> Hostile then onPeaceBroken state else state in
            let state = if sourceIsPlayer then UpdatePlayer.xpAdd (Cr.xpOnKill c) state else state in
            let state = onCreatureDeath c state in
            let m = SL.map state in
            let t = Matrix.get m p in
            state, Map.{ t with occupant = None; items = deathDrops @ t.items }
        else
            let hostilityOld = c.hostility in
            let state, hostility = if n >= 0 || not sourceIsPlayer then state, c.hostility else
                match c.hostility with
                | Docile -> onPeaceBroken state, Docile
                | Hostile -> state, Hostile
                | Peaceful -> onPeaceBroken state, Hostile
                | Tame -> onPeaceBroken state, if Random_.oneIn 3 then Hostile else Tame
            in
            let _ = if hostility <> hostilityOld then S.msgAdd state (C.sf "The %s gets angry!" c.info.name) in
            let m = SL.map state in
            let c = Map.getCreatureAt m p in
            let c' = Map.Creature
                { c with hp = c.hp + n
                ; hostility
                }
            in
            let t = Matrix.get m p in
            state, { t with occupant = Some c' }
    in
    let m = SL.map state in
    let m' = Matrix.set t' p m in
    SL.setMap m' state
