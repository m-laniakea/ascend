module C = Common

val add : Item.t list -> Item.t -> Item.t list
val concat : Item.t list -> Item.t list -> Item.t list
val remove : Item.t list -> Item.t -> C.howMany -> Item.t list
val split : Item.t list -> Item.t -> C.howMany -> Item.t * Item.t list
val splitIndex : Item.t list -> int -> C.howMany -> Item.t * Item.t list
val takeSelection : Item.t list -> C.selectionItem list -> Item.t list * Item.t list
