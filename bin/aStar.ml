module Astar :
sig
  type 'a t =
    {
      cost : 'a -> 'a -> int;
      goal : 'a;
      get_next_states : 'a -> 'a list;
    }

  (** [search problem start] returns a path (a list of states) from [start] to
      [problem.goal]. The path minimizes [problem.cost]. *)
  val search : 'a t -> 'a -> ('a list) option
end = struct
  type 'a t =
    {
      cost : 'a -> 'a -> int;
      goal : 'a;
      get_next_states : 'a -> 'a list;
    }

  type 'a path =
    {
      cost_from_start : int; (** the cost from the start to [head]. *)
      total_cost : int; (** the total cost from the start to the goal. *)
      head : 'a;
      tail : 'a list;
    }

  let create_path ?from problem state =
    let (cost_from_start, tail) = match from with
      | None -> (0, [])
      | Some p -> (p.cost_from_start + problem.cost p.head state,
                   p.head :: p.tail)
    in
    let total_cost = cost_from_start + problem.cost state problem.goal in
    { cost_from_start; total_cost; tail; head = state }

  (** [better p q] returns [true] if path [p] is better than path [q]. *)
  let better p q = p.total_cost < q.total_cost

  (** [pickup_eq_path p l] returns [Some (q, l')] where [q] is the path that
      indicates the same position as [p] and [l] is a list excluding [q]. *)
  let pickup_eq_path p l =
    match List.partition (fun q -> p.head = q.head) l with
    | [], _ -> None
    | [q], l' -> Some (q, l')
    | _ -> failwith "duplicated paths in open/close list"

  (** [trace_next_states problem open_list close_list path] traces the next
      states of [path.head].
      @return [(open_list', close_list')] where [open_list'] and [close_list']
      are respectively an open list and a close list after all of the next
      states are traced. *)
  let trace_next_states problem ol0 cl0 path0 =
    let trace_state (ol, cl) state =
      let path = create_path ~from:path0 problem state in
      match pickup_eq_path path ol with
      | Some (q, ol') -> if better path q then (path :: ol', cl) else (ol, cl)
      | None ->
        match pickup_eq_path path cl with
        | Some (q, cl') -> if better path q then (path :: ol, cl') else (ol, cl)
        | None -> (path :: ol, cl)
    in
    List.fold_left trace_state (ol0, cl0) (problem.get_next_states path0.head)

  (** [pickup_best_path l] returns [Some (p, l')] where [p] is the path that
      has the least cost in [l] and [l'] is an open list without [p]. *)
  let pickup_best_path = function
    | [] -> None
    | h :: t ->
      let aux (y, l) x = if better y x then (y, x :: l) else (x, y :: l) in
      Some (List.fold_left aux (h, []) t)

  let search problem start =
    let rec aux (ol, cl) =
      match pickup_best_path ol with
      | None -> None (* No path reaches to [problem.goal] *)
      | Some (p, ol') ->
        if p.head = problem.goal then Some p (* reached to the goal *)
        else aux (trace_next_states problem ol' (p :: cl) p)
    in
    match aux ([create_path problem start], []) with
    | None -> None
    | Some p -> Some (p.head :: p.tail)
end

(* ========================================================================== *
 * Domain-specific functions
 * ========================================================================== *)

