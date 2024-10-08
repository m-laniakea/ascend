open Matrix

module L = List

module Cr = Creature
module P = Position
module S = State
module SL = StateLevels

let playerD2Sight = 6

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

let canSeeViaTelepathy ~isTelepath pAt (state : S.t) =
    if not isTelepath then false else
    if pAt = state.player.pos then true else
    let m = SL.map state in

    match Map.getCreatureAtOpt m pAt with
    | None -> false
    | Some c -> not (Cr.isMindless c.info.attributes)

let canSee distance2Sight ~isBlind ~isTelepath from toSee state =
    if canSeeViaTelepathy ~isTelepath toSee state then true else
    if isBlind then false else

    let d = P.distance2 from toSee in
    if d > distance2Sight && not (SL.isLit toSee state) then false else
    let pathTo = getPathRay from toSee in
    let m = SL.map state in
    rayCanHitTarget m (Matrix.get m from) pathTo

let playerCanSee (state : S.t) toSee =
    let pp = state.player.pos in
    if pp = toSee then true else
    let attr = state.player.attributes in
    let isBlind = Cr.isBlind attr in
    let isTelepath = isBlind && Cr.isTelepath attr in
    canSee playerD2Sight ~isBlind ~isTelepath pp toSee state

let creatureCanSee (c : Creature.t) from toSee state =
    let isBlind = Cr.isBlind c.info.attributes in
    let isTelepath = Cr.isTelepath c.info.attributes in

    canSee 36 ~isBlind ~isTelepath from toSee state
