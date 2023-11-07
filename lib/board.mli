(** board.ml
    Specification of the structure of the game board and its characteristics; functions related to movement verification, display, etc.
*)

(** {1 Board Constants} *)

val board_size : int
(** The size of the board in both length and width. *)

(** {1 Board Validation Functions} *)

val is_valid_position : int * int -> bool
(** [is_valid_position pos] checks if the given position [pos] is valid on the game board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is within the bounds of the board, false otherwise.
*)

val is_wall_position : int * int -> bool
(** [is_wall_position pos] determines if a given position [pos] is valid for a wall on the board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is valid for a wall, false otherwise.
    @raise InvalidPosition if the position is not valid on the board.
*)

val is_player_position : int * int -> bool
(** [is_player_position pos] determines if a given position [pos] is valid for a player on the board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is valid for a player, false otherwise.
    @raise InvalidPosition if the position is not valid on the board.
*)

(** {1 Cell Content Accessors} *)

val get_cell_content : int * int -> 'a array array -> 'a
(** [get_cell_content pos board] retrieves the content of the cell at the given [pos] on the [board].
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @param board is the 2D array representing the game board.
    @return the content of the cell at the specified position.
    @raise OutOfBounds if the provided position is not valid on the board.
*)

val is_wall : Types.cell_content -> bool
(** [is_wall cell] determines if a given [cell] represents a wall on the board.
    @param cell is the cell content to check.
    @return true if the cell is a wall, otherwise false.
*)

val is_player : Types.cell_content -> bool
(** [is_player cell] determines if a given [cell] represents a player on the board.
    @param cell is the cell content to check.
    @return true if the cell is a player, otherwise false.
*)

(** {1 Movement Utilities} *)

val move_vectors : (int * int) list
(** List defining the potential movement vectors from a position. *)

val list_of_walls :
  int * int -> Types.cell_content array array -> (int * int) list
(** [list_of_walls pos board] produces a list of wall positions adjacent to a given position.
    @param pos is the starting position for which we want to find adjacent walls.
    @param board is the current game board.
    @return a list containing positions (coordinates) of adjacent walls.
    @raise OutOfBounds if the provided position is outside the board boundaries.
    @raise InvalidPlayerPosition if the provided position itself is not a player position.
*)

val list_of_players :
  int * int -> Types.cell_content array array -> (int * int) list
(** [list_of_players pos board] returns a list of positions of players 
    around the specified position [pos] on the [board], considering direct 
    neighbors two steps away in the vertical and horizontal directions.
    @param pos is the position around which we are searching for players.
    @param board is the current game board.
    @raise OutOfBounds if the given position is outside of the board boundaries.
    @raise InvalidPlayerPosition if the given position is not a player position.
*)

val is_wall_between :
  int * int -> int * int -> Types.cell_content array array -> bool
(** [is_wall_between pos1 pos2 board] checks if there's a wall between 
    two positions [pos1] and [pos2] on the [board].
    @param pos1 is the first position to check.
    @param pos2 is the second position to check.
    @param board is the current game board.
    @return true if there is a wall between pos1 and pos2, false otherwise.
    @raise OutOfBounds if either of the positions is outside the board boundaries.
    @raise InvalidPosition if positions are not even coordinates.
*)

val list_of_moves :
  int * int -> Types.cell_content array array -> (int * int) list
(** [list_of_moves pos board] calculates the possible moves for a player at 
    position [pos] on the [board]. The moves are determined based on the neighboring 
    walls, players, and empty cells.
    @param pos is the current position of the player on the board.
    @param board represents the current state of the game board.
    @return a list containing the possible move positions for the player.
    @raise OutOfBounds if the position is outside the board boundaries.
    @raise InvalidPlayerPosition if the position is not a player position.
*)

(** {1 Path Finding Functions} *)

val dfs_path_exists : Types.player -> Types.cell_content array array -> bool
(** [dfs_path_exists player board] determines if there's a path from the player's 
    [start_pos] to the target position on the [board] using a depth-first search (DFS). 
    @param player is the player whose position is the starting point for the DFS.
    @param board is the current game board.
    @return [true] if a path exists, [false] otherwise.
    @raise [OutOfBounds] if the start position is outside the board boundaries.
    @raise [InvalidPlayerPosition] if the start position is not a player position.
*)

(** {1 Wall Placement Functions} *)

val place_wall :
  int * int ->
  int * int ->
  Types.player list ->
  Types.cell_content array array ->
  Types.cell_content array array
(** [place_wall pos1 pos2 players board] attempts to place a wall on the [board] at [pos1] and [pos2] 
    without blocking any player in [players]. 
    @param pos1 is one end of the position where the wall is to be placed.
    @param pos2 is the other end of the position where the wall is to be placed.
    @param players is the list containing all players.
    @param board represents the current state of the game board.
    @return a new board state with the wall placed if the placement is valid.
    @raise [OutOfBounds] if the position is outside the board boundaries.
    @raise [InvalidWallPosition] if the given position is not a wall position.
    @raise [InvalidWallPlacement] if wall placement is invalid or blocks a player's path.
*)

(** {1 Printing Functions} *)

val print_player : Types.player -> unit
(** [print_player p] prints the color of player [p] to the standard output.
    @param p is the player whose color is to be printed.
*)

val print_cell : Types.cell_content -> unit
(** [print_cell cell] prints the content of a cell to the standard output.
    @param cell is the cell whose content is to be printed.
*)

val print_row : Types.cell_content array -> unit
(** [print_row row] prints a row of cells to the standard output.
    @param row is the array of cells to be printed.
*)

val print_board : Types.cell_content array array -> unit
(** [print_board board] prints the game board to the standard output.
    @param board is the game board to be printed.
*)
