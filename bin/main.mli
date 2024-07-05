open Common
open Matrix

type tile
type terrain
type state

type room

val contains : 'a list -> 'a -> bool

val charOfTerrain : tile Matrix.t -> pos -> tile -> string

val roomsGen : unit -> room list
val terrainAddHallways : room list -> tile Matrix.t -> tile Matrix.t
val terrainAddRoom : tile Matrix.t -> room -> tile Matrix.t
val terrainAddRooms : room list -> tile Matrix.t -> tile Matrix.t

val terrainAddHallways : room list -> tile Matrix.t -> tile Matrix.t

val getCurrentLevel : state -> tile Matrix.t
val setCurrentLevel : tile Matrix.t -> state -> state

