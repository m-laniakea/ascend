module C = Common
module R = Random_
module S = State

let rec addHpMax n (sp : S.player) =
    assert (n >= 0);
    if n = 0 then sp else

    let hpBonus = (R.rn 1 8 + 1 |> max 3) in
    let hpMax = sp.hpMax + hpBonus in
    let hp = sp.hp + hpBonus in

    addHpMax (n - 1) { sp with hp; hpMax }

let rec addAc n (sp : S.player) =
    assert (n >= 0);
    if n = 0 then sp else

    addAc (n - 1) { sp with acBonus = sp.acBonus + 1 }

let rec getLevel xp =
    let levelMax = 17 in
    (* Level xp thresholds are 2^(lvl - 1) * 10 *)
    (* Add 3 just to make level 1 -> 2 slightly easier *)
    let rec aux i =
        if (1 lsl i) * 10 > xp + 3 then
            i + 1 |> min levelMax
        else
            aux (i + 1)
    in
    aux 0

let levelTo levelNew (sp : S.player) =
    let lDiff = levelNew - sp.level in
    assert (lDiff > 0);
    (* TODO level down *)
    let sp =
        addHpMax lDiff sp
        |> addAc lDiff
    in
    { sp with level = levelNew }

let add n (state : S.t) =
    let sp = state.statePlayer in
    let xp = sp.xp + n |> max 0 in
    let levelNew = getLevel xp in

    let sp = if levelNew <> sp.level then
        let _ = S.msgAdd state (C.sf "Welcome to level %i!" levelNew) in
        levelTo levelNew sp
    else
        sp
    in
    let statePlayer = { sp with xp } in

    { state with statePlayer }
