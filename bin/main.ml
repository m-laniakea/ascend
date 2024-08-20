module Term = Notty_unix.Term

module N = Notty

let term = Term.create ()

let update event (state : State.t) = match (state.mode : State.mode) with
    | Dead -> Update.modeDead event state
    | Playing -> Update.modePlaying event state
    | Selecting s -> Update.modeSelecting event state s

let () =
    let rec go state =
        View.imageCreate state;

        match Term.event term with
        | `End | `Key (`ASCII 'C', [`Ctrl]) -> ()
        | `Resize _ -> go state
        | event as e -> match update e state with
            | Some s -> go s
            | None -> ()
    in
    go Init.stateInitial
