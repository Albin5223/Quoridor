val boardSize : int
val wallStart : int
val board : Types.cell_content list list
val playerTest : Types.player
val game : Types.game
val isValidPosition : int * int -> bool
val getCellContent : int * int -> Types.cell_content
val isWall : Types.cell_content -> bool
val isPlayer : Types.cell_content -> bool
val isWallPosition : int * int -> bool
val getX : 'a * 'b -> 'a
val getY : 'a * 'b -> 'b
val listOfWalls : int * int -> (int * int) list
val listOfMoves : int * int -> (int * int) list
val canPlaceHorizontalWall : Types.game -> int * int -> bool
val canPlaceVerticalWall : Types.game -> int * int -> bool
val placeHorizontalWall : Types.game -> int * int -> Types.game
val placeVerticalWall : Types.game -> int * int -> Types.game
val print_board: Types.cell_content list list -> unit