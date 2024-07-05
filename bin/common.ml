type pos =
    { row : int
    ; col : int
    }

let listMake n v = List.init n (fun _ -> v)

let listSet i v =
    List.mapi (fun ci ov -> if ci <> i then ov else v)
