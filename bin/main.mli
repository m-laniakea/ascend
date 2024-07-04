type pos
type 'a matrix
type tileTerrain
type typeTerrain
type state 

type room


val contains : 'a list -> 'a -> bool
val matrixFill : int -> int -> 'a -> 'a matrix
val matrixSet : 'a -> pos -> 'a matrix -> 'a matrix
val matrixMap : ('a -> 'b) -> 'a matrix -> 'b matrix
val matrixIMap : ('a matrix -> pos -> 'a -> 'b) -> 'a matrix -> 'b matrix
val matrixGet : 'a matrix -> pos -> 'a option
val matrixFlatten : 'a matrix -> 'a list

val charOfTerrain : tileTerrain matrix -> pos -> tileTerrain -> string 

val roomsGen : unit -> room list
val terrainAddRoom : tileTerrain matrix -> room -> tileTerrain matrix
val terrainAddRooms : room list -> tileTerrain matrix -> tileTerrain matrix

val terrainAddHallways : room list -> tileTerrain matrix -> tileTerrain matrix

val getCurrentLevel : state -> tileTerrain matrix
val setCurrentLevel : tileTerrain matrix -> state -> state

