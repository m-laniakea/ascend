module C = Common
module P = Position

module Matrix  =
struct

type 'a t = ('a list) list

let raw m = m

let ofRaw = function
    | [] -> failwith "Matrix may not be empty"
    | [[]; _] -> failwith "Row may not be empty"
    | m ->
        let cols = List.hd m |> List.length in
        let rowsSameLength = List.for_all (fun r -> List.length r = cols) m in
        match rowsSameLength with
        | true -> m
        | false -> failwith "Matrix rows must be same length"

let fill (size : P.dim_2D) v =
    assert (size.rows > 0);
    assert (size.cols > 0);
    C.listMake size.rows (C.listMake size.cols v)

let set v (pos : P.t) = List.mapi
    ( fun ri r ->
        if ri <> pos.row then
            r
        else
            C.listSet pos.col v r
    )

let get m (pos : P.t) =
    let r = List.nth m pos.row in
    List.nth r pos.col

let getOpt m (pos : P.t) =
    if pos.row < 0 || pos.col < 0 then None else
        match List.nth_opt m pos.row with
        | None -> None
        | Some r -> List.nth_opt r pos.col

let map f = List.map (List.map f)

let mapI f m = List.mapi
    ( fun ri r ->
        List.mapi
            ( fun ci cv ->
                f m P.{ row = ri; col = ci } cv
            )
            r
    )
    m

let flatten = List.flatten

let fold f acc m =
    flatten m
    |> List.fold_left f acc

let foldI f acc m =
    mapI
    ( fun _ p v ->
        p, v
    ) m
    |> fold (fun acc (p, v) -> f m p acc v) acc

let mapIFindAll f m =
    foldI
        ( fun _ p acc t -> match f m p t with
            | Some v -> v::acc
            | None -> acc
        )
        [] m

end
