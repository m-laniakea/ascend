open Matrix

module L = List
module P = Position
module S = State
module SL = StateLevels

let playerD2Sight = 9

let getPathRay from target =
    let rec aux path =
        let h = List.hd path in

        if h = target then
            List.rev path
        else

        let next =
            Map.posAround h
            |> L.sort (Position.closestTo target)
            |> L.hd
        in

        aux (next::path)
    in
    aux [from]

let rec rayCanHitTarget m (prev : Map.tile) path =
    let (t : Map.tile) = Matrix.get m (List.hd path) in match path with
    | [] -> true
    | _::[] ->
        ( match t.t with
            | Wall _ when prev.t = Hallway HallRegular -> false
            | Door (Hidden, _) when prev.t = Hallway HallRegular -> false
            | _ -> true
        )
    | _::tl -> match t.occupant with
        | Some Boulder -> false
        | Some Player -> rayCanHitTarget m t tl
        | Some (Creature _) ->
            (* ^TODO assumes occupants can't be in solid tiles *)
            rayCanHitTarget m t tl
            (* TODO large occupants *)
        | None -> match t.t with
            | Floor | Hallway HallRegular
            | StairsDown | StairsUp
            | Door (Broken, _)
            | Door (Open, _) -> rayCanHitTarget m t tl
            | Door (Closed, _) | Door (Hidden, _) -> false
            | Hallway HallHidden -> false
            | Stone -> false
            | Unseen -> false
            | Wall _ -> false

let canSee distance2Sight from toSee state =
    let d = P.distance2 from toSee in
    if d > distance2Sight && not (SL.isLit toSee state) then
        false
    else
    let pathTo = getPathRay from toSee in
    let m = SL.map state in
    rayCanHitTarget m (Matrix.get m from) pathTo

let playerCanSee (state : S.t) toSee =
    let pp = state.player.pos in
    (* TODO blindness *)
    canSee playerD2Sight pp toSee state

let creatureCanSee _ from toSee state =
    (* TODO use creature info *)
    canSee 36 from toSee state

