open Matrix

type tile
type terrain

type state

type room

val roomsGen : unit -> room list
val terrainAddRoom : tile Matrix.t -> room -> tile Matrix.t
val terrainAddRooms : room list -> tile Matrix.t -> tile Matrix.t

val terrainAddHallways : room list -> tile Matrix.t -> tile Matrix.t

