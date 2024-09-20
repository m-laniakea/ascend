module C = Common

let setStack n i =
    assert (n > 0);

    let open Item in
    let stack = match i.stats.stack with
        | NonStackable when n <> 1 -> failwith "tried to set stack of non-stackable item"
        | NonStackable -> NonStackable
        | Stack _ -> Stack n
    in
    let stats = { stack } in
    { i with stats }

let combine a b =
    let open Item in
    let stack = match a.stats.stack, b.stats.stack with
        | Stack m, Stack n -> Stack (m + n)
        | _ -> failwith "Tried to stack unstackables"
    in
    let stats = { stack } in
    { a with stats }


let substract from howMany =
    let countRemain = match howMany with
        | C.All -> 0
        | Count c -> (Item.count from) - c
    in

    match countRemain with
    | _ when countRemain < 0 -> failwith "Asked to remove too many of item"
    | c when c = 0 -> None
    | c ->
        let open Item in
        let stack = Stack c in (* non-zero, so cannot be NonStackable *)
        let stats = { stack } in
        Some { from with stats }

let add items item =
    if items |> List.exists (Item.canStack item) |> not then item::items else

    let result, _ =
        items
        |> List.fold_left
            (fun (result, hasCombined) i -> match item, i with
            | _ when hasCombined -> i::result, true
            | _ when not (Item.canStack item i) -> i::result, false
            | a, b ->
                (combine a b)::result, true
            )
            ([], false)
    in
    List.rev result

let concat from into =
    List.fold_left
    (fun result item -> add result item)
    into
    from

let remove items item howMany =

    let index = match List.find_index (Item.equal item) items with
        | None -> failwith "asked to remove item not in list"
        | Some i -> i
    in
    let toRemoveFrom = List.nth items index in
    let others = List.filteri (fun i _ -> i <> index) items in

    match substract toRemoveFrom howMany with
    | None -> others
    | Some i -> i::others

let split items item howMany =
    let count = match howMany with
        | C.All -> Item.count item
        | Count n -> n
    in
    let item = setStack count item in
    let items = remove items item howMany in
    item, items

let splitIndex items index howMany =
    let item = List.nth items index in
    split items item howMany

let takeSelection items sil =
    let toTakeWithCount = List.map
        ( fun (si : C.selectionItem) ->
            let item = List.nth items si.iIndex in
            item, si.howMany
        )
        sil
    in
    List.fold_left
    (fun (taken, remain) (item, howMany) ->
        let item, remain = split remain item howMany in
        item::taken, remain
    )
    ([], items)
    toTakeWithCount
