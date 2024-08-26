module L = List

let pointsSpeedPerTurn = 12

let brown = Notty.A.yellow

let sf = Format.sprintf

let id a = a

let listMake n v = List.init n (fun _ -> v)

let partitionI f l =
    L.mapi (fun i v -> i, v) l
    |> L.partition_map (fun (i, v) -> if f i v then Left v else Right v)

let listTake n = L.filteri (fun i _ -> i < n)

let listSet i v =
    List.mapi (fun ci ov -> if ci <> i then ov else v)

let hdOpt l = List.nth_opt l 0

let contains l v = List.mem v l

let range min max = List.init (max - min + 1) (fun i -> i + min)

let listRemove v l = match L.find_index ((=) v) l with
    | None -> l
    | Some i -> partitionI (fun ix _ -> ix <> i) l |> fst

type selectionItem =
    { iIndex : int
    ; name : string
    ; selected : bool
    ; letter : char
    }
