type pos
type 'a matrix
type tile
type terrain
type state

type room


val contains : 'a list -> 'a -> bool
val matrixFill : int -> int -> 'a -> 'a matrix
val matrixSet : 'a -> pos -> 'a matrix -> 'a matrix
val matrixMap : ('a -> 'b) -> 'a matrix -> 'b matrix
val matrixIMap : ('a matrix -> pos -> 'a -> 'b) -> 'a matrix -> 'b matrix
val matrixGet : 'a matrix -> pos -> 'a
val matrixGetOpt : 'a matrix -> pos -> 'a option
val matrixFlatten : 'a matrix -> 'a list

val charOfTerrain : tile matrix -> pos -> tile -> string

val roomsGen : unit -> room list
val terrainAddHallways : room list -> tile matrix -> tile matrix
val terrainAddRoom : tile matrix -> room -> tile matrix
val terrainAddRooms : room list -> tile matrix -> tile matrix

val terrainAddHallways : room list -> tile matrix -> tile matrix

val getCurrentLevel : state -> tile matrix
val setCurrentLevel : tile matrix -> state -> state

