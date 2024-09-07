module P = Position

module type Matrix_t =
sig

type 'a t

val raw : 'a t -> 'a list list
val ofRaw : 'a list list -> 'a t

val fill : P.dim_2D -> 'a -> 'a t
val set : 'a -> P.t -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val mapI : ('a t -> P.t -> 'a -> 'b) -> 'a t -> 'b t
val get : 'a t -> P.t -> 'a
val getOpt : 'a t -> P.t -> 'a option
val flatten : 'a t -> 'a list
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val foldI : ('a t -> P.t -> 'acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val mapIFindAll : ('a t -> P.t -> 'a -> 'b option) -> 'a t -> 'b list

end
