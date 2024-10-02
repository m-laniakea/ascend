open Matrix

module L = List

module Term = Notty_unix.Term
module N = Notty
module A = N.A
module I = N.I

module C = Common
module Cr = Creature
module P = Position
module S = State
module SL = StateLevels

let term = Term.create () (* TODO shouldn't be created a second time here *)

let onBlack style s = I.string A.(Config.styleBg ++ style) s

let imageOfItem ?(styles=A.(st bold)) (i : Item.t) =
    let color, symbol = match i.t with
    | Comestible c -> c.color, "%"
    | Container _ -> C.brown, "("
    | Corpse c -> c.color, "%"
    | Gold -> A.lightyellow, "$"
    | Potion _ -> A.white, "!"
    | Rock -> A.white, "*"
    | Scroll _ -> A.white, "?"
    | Weapon w -> w.color, ")"
    | Wand _ -> A.cyan, "/"
    in
    onBlack A.(styles ++ fg color) symbol

let imageOfTile (state : S.t) _ p = function
    | Map.{ occupant = Some occ; _ } ->
        ( match occ with
            | Creature c ->
                onBlack A.(st bold ++ fg c.info.color) c.info.symbol
            | Player -> onBlack A.(st bold ++ fg lightwhite) "@"
            | Boulder -> onBlack A.(st bold ++ fg white) "0"
        )
    | { items = topItem::others; _ } ->
        let styles = if others = [] then A.(st bold) else A.(bg lightblack ++ st bold) in
        imageOfItem ~styles topItem
    | t ->
        let c = match t.t with
        | Stone -> " "
        | Unseen -> " "
        | Hallway HallHidden -> " "
        | Hallway HallRegular -> "#"
        | Floor -> "."
        | Door (Broken, _) -> "."
        | Door (Closed, _) -> "+"
        | Door (Open, Horizontal) | Door (Hidden, Vertical) -> "|"
        | Door (Open, Vertical) | Door (Hidden, Horizontal) -> "-"
        | StairsUp -> "<"
        | StairsDown -> ">"
        | Wall Horizontal -> "-"
        | Wall Vertical -> "|"
        in
        let attr = state.player.attributes in
        let playerNotBlind = not (Cr.isBlind attr) in
        let color = if playerNotBlind && (SL.isLit p state || Sight.playerCanSee state p) then
                A.white
            else
                if "#" = c then A.lightblack else A.black
        in
        onBlack A.(fg color) c

let applyAnimatedTiles animationLayer m =
    animationLayer
    |> List.fold_left
        ( fun m (pos, styledChar) ->
            Matrix.set styledChar pos m
        )
        m

let imageCreate ?(animationLayer=[]) (state : S.t) =
    let open Notty.Infix in
    let sp = state.player in
    let header (state : S.t) =
        Format.sprintf "HP: %i/%i | Depth: %i | AC: %i | XP: %i | Level: %i" sp.hp sp.hpMax state.levels.indexLevel (StatePlayer.ac state) sp.xp sp.level
        |> I.string A.empty
    in
    let footer (state : S.t) =
        I.string A.empty (C.sf "$ %i" state.player.gold)
    in
    let getInventoryValue (state : S.t) =
        let sp = state.player in
        let wielded = Option.fold ~none:[] ~some:(fun w -> [w]) sp.weaponWielded in
        let inventory = wielded @ sp.inventory in
        let hasScepter = StatePlayer.hasScepter state in

        let items = if hasScepter then Items.remove inventory Item.scepterOfYorel C.(Count 1) else inventory in

        L.map Item.getPriceBase items |> L.fold_left (+) 0
    in

    let messageDeath (state : S.t) =
        let gold = state.player.gold in
        let valItems = getInventoryValue state in
        [ ""
        ; "You are dead."
        ; C.sf "You died on level %i." state.levels.indexLevel
        ; C.sf "You were carrying %i gold and %i gold worth of items." gold valItems
        ; "Farewell."
        ]
        |> L.map (I.string A.(st bold))
        |> I.vcat
    in
    let messageVictory (state : S.t) =
        let sp = state.player in
        let statsKilled =
            let tk = sp.timesKilled in
            if 0 = tk then
                "without ever dying"
            else
                C.sf "having died %i time%s" tk (C.plural tk)
        in
        let gold = state.player.gold in
        let valItems = getInventoryValue state in

        let storyPet =
            let m = SL.map state in
            match Map.getPets m with
            | [] -> []
            | c::_ ->
                [ C.sf "Your pet %s zooms gleefully through the lush grass and around you," c.info.name
                ; "before munching on a giant carrot!"
                ; ""
                ]
        in

        let isVictoryFlawless =
            (* TODO other conditions *)
            0 = sp.timesKilled
        in

        let storyBonus = match isVictoryFlawless with
            | false -> []
            | true ->
                [ "Rievax the Revelator appears before you."
                ; ""
                ; "You smile at each other, and you begin raising your hands to offer the Scepter,"
                ; "but the Revelator motions for you to stop."
                ; "\"No. My time has come and may go again soon. The Scepter is yours, as it always has been."
                ; "Use your gifts wisely, for the benefit of all.\""
                ; ""
                ]
        in

        [ "The scepter bursts open the hatch to the dungeon!"
        ; "You hear the voice of Gnilsog gasp 'NO!' as its soul is scattered back into the void."
        ; ""
        ; "You cautiously ascend the ladder, the air sweet, the gentle breeze invigorating."
        ; "The sun shines benevolently upon you, warming away the chill you have felt for far too long."
        ; ""
        ; "The former forces of Gnilsog kneel respectfully before you."
        ; ""
        ; "Goodbye, hero!"
        ; ""
        ; C.sf "You entered the Dungeons %i turn%s ago." state.turns (C.plural state.turns)
        ; C.sf "You are carrying %i gold and %i gold worth of items." gold valItems
        ; C.sf "You are level %i with %i XP" sp.level sp.xp
        ; C.sf "You ascended %s." statsKilled
        ; ""
        ]
        @ storyPet
        @ storyBonus
        @
        [ "Farewell."
        ]
        |> L.map (I.string A.empty)
        |> I.vcat
    in
    let messages (state : S.t) =
        Queue.fold (fun i m -> i <-> (I.string A.empty m)) I.empty state.messages
    in
    let imageOfSelectionItem (si : C.selectionItem) =
        let c = match si.howMany with
            | _ when not si.selected -> "-"
            | Count _ -> "#"
            | All -> "+"
        in
        let text = C.sf "%c %s %s" si.letter c si.name in
        I.string A.empty text
    in
    ( match state.mode with
        | Dead ->
            header state
            <-> messages state
            <-> messageDeath state
        | DisplayText s ->
            s @ [""; ""; "Press <space> to continue..."]
            |> L.map (I.string A.empty)
            |> I.vcat

        | Playing ->
            header state
            <->
            (
                let mView =
                    S.getKnowledgeCurrentMap state
                    |> Matrix.mapI (imageOfTile state)
                    |> applyAnimatedTiles animationLayer
                in
                I.tabulate Map.size.cols Map.size.rows (fun c r -> Matrix.get mView { row = r; col = c })
            )
            <-> footer state
            <|> messages state

        | Selecting s ->
                ( match s with
                | SelectDir _ ->
                    [ "y  k  u"
                    ; " \\ | / "
                    ; "h  @  l"
                    ; " / | \\"
                    ; "b  j  n"
                    ]
                    |> L.map (I.string A.empty)
                    |> I.vcat

                | SelectItems s ->
                    s.sItems
                    |> L.sort (fun (s : C.selectionItem) s'-> Char.compare s.letter s'.letter)
                    |> L.map imageOfSelectionItem
                    |> I.vcat
                )
        | Victory ->
            messageVictory state
    )
    |> Term.image term

type animation =
    { dir : P.dir
    ; posStart : P.t
    ; posCurrent : P.t
    ; posEnd : P.t
    ; image : N.image
    }

let rec animate state ?(cumulative=true) ?(linger=true) ?(animationLayer=[]) a =
    let animationLayer' = match cumulative with
        | _ when a.posCurrent = a.posStart -> []
        | true -> (a.posCurrent, a.image)::animationLayer
        | false -> [a.posCurrent, a.image]
    in
    imageCreate ~animationLayer:animationLayer' state;
    Unix.sleepf (if cumulative then 0.05 else 0.10);

    if a.posCurrent = a.posEnd then
        if linger then Unix.sleepf 0.3 else ()
    else
    let posCurrent = P.step a.posCurrent a.dir in
    let a' = { a with posCurrent } in

    animate state ~cumulative ~linger ~animationLayer:animationLayer' a'
