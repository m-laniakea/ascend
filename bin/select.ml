open Matrix

module L = List

module C = Common
module P = Position
module S = State
module SL = StateLevels

let selectionOfItems ~single oc l =
    L.filter_map C.id l
    |> C.listTake 26
    |> L.mapi
        C.
        ( fun ix (iix, i) ->
            { letter = 0x61 (* 'a' *) + ix |> Char.chr
            ; iIndex = iix
            ; name = Item.nameDisplay i
            ; selected = false
            }
        )
    |>  ( fun l ->
            S.
            { sItems = l
            ; single
            ; onComplete = oc
            }
        )

let handleItems k (s : S.selectItems) state = match k with
    | ' ' ->
        let selected = L.filter (fun (s : C.selectionItem) -> s.selected) s.sItems in
        let nSelected = L.length selected in
        if nSelected = 0 || s.single && nSelected > 1 then
            (* TODO give feedback to player *)
            Some state
        else
        let firstSelected = List.hd selected in
        let state = S.{ state with mode = Playing } in
        ( match s.onComplete with
            | DoDrop -> Player.action (Drop selected) state
            | DoPickup -> Player.action (Pickup selected) state
            | DoQuaff -> Player.action (Quaff firstSelected) state
            | DoRead -> Player.action (Read firstSelected) state
            | DoWield -> Player.action (Wield firstSelected) state
            | SelectDirThrow ->
                    let mode = S.Selecting (SelectDir (DoThrow firstSelected)) in
                    Some { state with mode }
            | SelectDirZap ->
                    let mode = S.Selecting (SelectDir (DoZap firstSelected)) in
                    Some { state with mode }

        )
    | ',' ->
        let hasUnselected = L.exists (fun (si : C.selectionItem) -> not si.selected) s.sItems in
        let selected = hasUnselected in
        let sItems = List.map (fun (si : C.selectionItem) -> { si with selected }) s.sItems in
        let mode = S.Selecting (SelectItems { s with sItems }) in

        Some { state with mode }

    | k ->
        match L.find_index (fun (si : C.selectionItem) -> k = si.letter) s.sItems with
        | None -> Some state
        | Some i ->
            let si = L.nth s.sItems i in
            let sItems = if s.single then
                L.map (fun (si : C.selectionItem) -> { si with selected = false }) s.sItems
            else
                s.sItems
            in
            let sItems = C.listSet i { si with selected = not (si.selected) } sItems in
            let mode = S.Selecting (SelectItems { s with sItems }) in
            Some { state with mode }

let handleDir k sd (state : S.t) =
    let maybeFDir = match k with
        | 'y' -> Some Map.northWest
        | 'u' -> Some Map.northEast
        | 'b' -> Some Map.southWest
        | 'n' -> Some Map.southEast
        | 'k' -> Some Map.north
        | 'j' -> Some Map.south
        | 'l' -> Some Map.east
        | 'h' -> Some Map.west
        | _ -> None
    in
    match maybeFDir with
        | None -> Some state
        | Some fDir ->
            let mode = S.Playing in
            let state = { state with mode } in
            let dir = fDir P.zero |> P.dir in
            ( match sd with
            | S.DoClose -> Player.action (Close fDir) state
            | DoThrow si -> Player.action (Throw (si, dir)) state
            | DoZap si -> Player.action (Zap (si, dir)) state
            (* TODO wand charge is still used up if direction is cancelled (Escape) *)
            )

let handle k s state = match s with
    | S.SelectDir sd -> handleDir k sd state
    | SelectItems si -> handleItems k si state

let dirClose (state : S.t) =
    let mode = S.Selecting (SelectDir DoClose) in
    Some { state with mode }

let drop (state : S.t) =
    let inv = state.statePlayer.inventory in
    let items = L.mapi (fun ix i -> Some (ix, i)) inv in
    let selection = selectionOfItems ~single:false DoDrop items in
    match items with
        | [] ->
            let _ = S.msgAdd state "You don't have anything you can drop." in
            Some state
        | _ ->
            let mode = S.Selecting (SelectItems selection) in
            Some { state with mode }

let pickup (state : S.t) =
    let pp = state.statePlayer.pos in
    let m = SL.map state in
    let t = Matrix.get m pp in
    let items = L.mapi (fun ix i -> Some (ix, i)) t.items in
    let selection = selectionOfItems ~single:false DoPickup items in
    match t.items with
        | [] ->
            let _ = S.msgAdd state "There's nothing to pick up here." in
            Some state
        | _::[] ->
            Player.action (Pickup selection.sItems) state
        | _ ->
            let mode = S.Selecting (SelectItems selection) in
            Some { state with mode }

let read (state : S.t) =
    let sp = state.statePlayer in
    let readables = L.mapi (fun ix i -> if Item.isReadable i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some readables) then Some state else
    let selection = selectionOfItems ~single:true DoRead readables in
    let mode = S.Selecting (SelectItems selection) in
    Some { state with mode }

let quaff (state : S.t) =
    let sp = state.statePlayer in
    let quaffables = L.mapi (fun ix i -> if Item.isQuaffable i then Some (ix, i) else None) sp.inventory in
    (* ^TODO refactor *)
    if not (L.exists Option.is_some quaffables) then Some state else
    let selection = selectionOfItems ~single:true DoQuaff quaffables in
    let mode = S.Selecting (SelectItems selection) in
    Some { state with mode }

let throw (state : S.t) =
    (* ^TODO refactor *)
    let sp = state.statePlayer in
    let throwables = L.mapi (fun ix i -> if Item.isWeapon i then Some (ix, i) else None) sp.inventory in
    (* TODO allow throwing non-weapons *)
    if not (L.exists Option.is_some throwables) then Some state else
    let selection = selectionOfItems ~single:true SelectDirThrow throwables in
    let mode = S.Selecting (SelectItems selection) in
    Some { state with mode }

let wield (state : S.t) =
    let sp = state.statePlayer in
    let wieldables = L.mapi (fun ix i -> if Item.isWeapon i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some wieldables) then Some state else
    let selection = selectionOfItems ~single:true DoWield wieldables in
    let mode = S.Selecting (SelectItems selection) in
    Some { state with mode }

let zap (state : S.t) =
    (* ^TODO refactor *)
    let sp = state.statePlayer in
    let zappables = L.mapi (fun ix i -> if Item.isZappable i then Some (ix, i) else None) sp.inventory in
    if not (L.exists Option.is_some zappables ) then Some state else
    let selection = selectionOfItems ~single:true SelectDirZap zappables in
    let mode = S.Selecting (SelectItems selection) in
    Some { state with mode }
