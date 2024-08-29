module C = Common

type uid = Int64.t * Int64.t

type roll =
    { rolls : int
    ; sides : int
    }

let rn min max = Random.int_in_range ~min ~max

let oneIn n = rn 0 (n - 1) = 0

let nToOne n = not (oneIn (n + 1))

let index l =
    assert (List.length l > 0);

    let iLast = List.length l - 1 in
    rn 0 iLast

let item l =
    assert (List.length l > 0);

    List.nth l (index l)

let relative l =
    let freqTotal =
        List.map (fun (_, freq) -> freq) l
        |> List.fold_left (+) 0
    in
    assert (freqTotal > 0);

    let rec aux ix = function
        | [] -> assert false
        | (i, freq)::_ when ix <= freq -> i
        | (_, freq)::tl -> aux (ix - freq) tl
    in
    let ix = rn 1 freqTotal in
    aux ix l

let roll roll =
    C.range 1 roll.rolls
    |> List.map (fun _ -> rn 1 roll.sides)
    |> List.fold_left (+) 0

let rollCompare a b =
    Int.compare (a.sides * a.rolls) (b.sides * b.rolls)

type attackLanded =
    | Miss
    | MissBarely
    | Hit

let rollAttackLanded threshold addSides =
    let roll = rn 1 (20 + addSides) in
    match roll with
    | _ when roll < threshold -> Hit
    | _ when roll = threshold -> MissBarely
    | _ -> Miss

let int64 () = Random.int64_in_range ~min:Int64.min_int ~max:Int64.max_int

let uid () = int64 (), int64 ()

let shuffle l =
    List.map (fun i -> int64 (), i) l
    |> List.sort compare
    |> List.map snd
