open Matrix

module L = List

module Term = Notty_unix.Term
module N = Notty
module A = N.A
module I = N.I

module C = Common
module P = Position
module S = State

let term = Term.create () (* TODO shouldn't be created a second time here *)

let imageOfItem ?(styles=A.(st bold)) (i : Item.t) = match i with
    | Container _ -> I.string A.(styles ++ fg C.brown) "("
    | Corpse c -> I.string A.(styles ++ fg c.color) "%"
    | Gold _ -> I.string A.(styles ++ fg lightyellow) "$"
    | Potion _ -> I.string A.(styles ++ fg white) "!"
    | Rock _ -> I.string A.(styles ++ fg white) "*"
    | Scroll _ -> I.string A.(styles ++ fg white) "?"
    | Weapon w -> I.string A.(styles ++ fg w.color) ")"
    | Wand _ -> I.string A.(styles ++ fg A.cyan) "/"

let imageOfTile _ _ = function
    | Map.{ occupant = Some occ; _ } ->
        ( match occ with
            | Creature c ->
                I.string A.(st bold ++ fg c.info.color) c.info.symbol
            | Player -> I.string A.(st bold ++ fg lightwhite) "@"
            | Boulder -> I.string A.(st bold ++ fg white) "0"
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
        | Door (Closed, _) -> "+"
        | Door (Open, Horizontal) | Door (Hidden, Vertical) -> "|"
        | Door (Open, Vertical) | Door (Hidden, Horizontal) -> "-"
        | StairsUp -> "<"
        | StairsDown -> ">"
        | Wall Horizontal -> "-"
        | Wall Vertical -> "|"
        in
        I.string A.(fg white) c

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
    let messageDeath (state : S.t) =
        let gold = state.player.gold in
        let valItems = L.map Item.getPriceBase state.player.inventory |> L.fold_left (+) 0 in
        [ ""
        ; "You are dead."
        ; C.sf "You died on level %i." state.levels.indexLevel
        ; C.sf "You were carrying %i gold and %i zorkmids worth of items." gold valItems
        ; "Farewell."
        ]
        |> L.map (I.string A.(st bold))
        |> I.vcat
    in
    let messages (state : S.t) =
        Queue.fold (fun i m -> i <-> (I.string A.empty m)) I.empty state.messages
    in
    ( match state.mode with
        | Dead ->
            header state
            <-> messages state
            <-> messageDeath state
        | Playing ->
            header state
            <->
            (
                let mView =
                    S.getKnowledgeCurrentMap state
                    |> Matrix.mapI imageOfTile
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
                    |> L.map (fun (s : C.selectionItem) -> I.string A.empty (C.sf "%c %s %s" s.letter (if s.selected then "+" else "-") s.name))
                    |> I.vcat
                )
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
