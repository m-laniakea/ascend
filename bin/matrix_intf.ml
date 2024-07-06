open Common

module type Matrix_t =
sig

type 'a t

val raw : 'a t -> 'a list list

val fill : pos -> 'a -> 'a t
val set : 'a -> pos -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val mapI : ('a t -> pos -> 'a -> 'b) -> 'a t -> 'b t
val get : 'a t -> pos -> 'a
val getOpt : 'a t -> pos -> 'a option
val flatten : 'a t -> 'a list
val fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val foldI : ('a t -> pos -> 'acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val mapIFindAll : ('a t -> pos -> 'a -> 'b option) -> 'a t -> 'b list

end
