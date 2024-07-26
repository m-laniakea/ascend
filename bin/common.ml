let sf = Format.sprintf

type pos =
    { row : int
    ; col : int
    }

let listMake n v = List.init n (fun _ -> v)

let listSet i v =
    List.mapi (fun ci ov -> if ci <> i then ov else v)

let contains l v = List.find_opt ((=) v) l |> Option.is_some

let rn min max = Random.int_in_range ~min ~max

let oneIn n = rn 0 (n - 1) = 0
