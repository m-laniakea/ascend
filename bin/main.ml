module Term = Notty_unix.Term

let term = Term.create ()

let () =
    let rec go state =
        View.imageCreate state;

        match Term.event term with
        | `End | `Key (`ASCII 'C', [`Ctrl]) -> ()
        | `Resize _ -> go state
        | event -> match Update.exec event state with
            | Some s -> go s
            | None -> ()
    in
    go Init.state
