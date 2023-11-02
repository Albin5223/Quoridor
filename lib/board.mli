(** Specification of the structure of the game board and its characteristics; 
    functions related to movement verification, display, etc. *)

val board_size : int
(** The size of the board in both length and width. *)

val is_valid_position : int * int -> bool
(** [is_valid_position pos] checks if the given position [pos] is valid on the game board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is within the bounds of the board, false otherwise.
*)

val get_cell_content : int * int -> 'a array array -> 'a
(** [get_cell_content pos board] retrieves the content of the cell at the given [pos] on the [board].
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @param board is the 2D array representing the game board.
    @return the content of the cell at the specified position.
    @raise raises OutOfBounds if the provided position is not valid on the board.
*)

val is_wall : Types.cell_content -> bool
(** [is_wall cell] determines if a given [cell] represents a wall on the board.
    @return true if the cell is a wall, otherwise false.
*)

val is_player : Types.cell_content -> bool
(** [is_player cell] determines if a given [cell] represents a player on the board.
    @return true if the cell is a player, otherwise false.
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

val move_vectors : (int * int) array
(** Array defining the potential movement vectors from a position *)

val list_of_walls :
  int * int -> Types.cell_content array array -> (int * int) array
(** [list_of_walls pos board] produces a list of wall positions adjacent to a given position.
    @param pos is the starting position for which we want to find adjacent walls.
    @param board is the current game board.
    @return an array containing positions (coordinates) of adjacent walls.
    @raise OutOfBounds if the provided position is outside the board boundaries.
    @raise InvalidWallPosition if the provided position itself is a wall position.
*)

val list_of_players :
  int * int -> Types.cell_content array array -> (int * int) array
(** [list_of_players pos board] returns an array of positions of players 
    around the specified position [pos] on the [board], considering direct 
    neighbors two steps away in the vertical and horizontal directions.
    @param pos is the position around which we are searching for players.
    @param board is the current game board.
    @raise OutOfBounds if the given position is outside of the board boundaries.
    @raise InvalidWallPosition if the given position is a wall position.
*)

val is_wall_between :
  int * int -> int * int -> Types.cell_content array array -> bool
(** [is_wall_between pos1 pos2 board] checks if there's a wall between 
    two positions [pos1] and [pos2] on the [board].
    @param pos1 is the first position to check.
    @param pos2 is the second position to check.
    @param board is the current game board.
    @raise OutOfBounds if either of the positions is outside the board boundaries.
    @raise InvalidPosition if positions are not even coordinates.
*)

val list_of_moves :
  int * int -> Types.cell_content array array -> (int * int) array
(** [list_of_moves pos board] calculates the possible moves for a player at 
    position [pos] on the [board]. The moves are determined based on the neighboring 
    walls, players, and empty cells.
    @param pos is the current position of the player on the board.
    @param board represents the current state of the game board.
    @return an array containing the possible move positions for the player.
    @raise [OutOfBounds] if the position is outside the board boundaries.
    @raise [InvalidWallPosition] if the position is a wall position.
*)

val dfs_path_exists : int * int -> Types.cell_content array array -> bool
(** [dfs_path_exists start_pos board] determines if there's a path from [start_pos] 
    to the target position on the [board] using a depth-first search (DFS). 
    @param start_pos is the starting position for the DFS.
    @param board is the current game board.
    @return [true] if a path exists, [false] otherwise.
    @raise [OutOfBounds] if the start position is outside the board boundaries.
*)

val can_place_wall :
  int * int -> (int * int) array -> Types.cell_content array array -> bool
(** [can_place_wall pos players_positions board] checks if a wall can be placed 
    at the specified [pos] without blocking any player in [players_positions] on the [board].
    @param pos is the position where the wall is to be placed.
    @param players_positions is the array containing the positions of all players.
    @param board represents the current state of the game board.
    @return [true] if the wall can be placed at [pos], [false] otherwise.
    @raise [OutOfBounds] if the position is outside the board boundaries.
    @raise [InvalidWallPosition] if the given position is not a wall position.
    @raise [InvalidWallPlacement] if wall placement is invalid or overlaps with an existing wall.
*)

val place_wall :
  int * int ->
  (int * int) array ->
  Types.cell_content array array ->
  Types.cell_content array array
(** [place_wall pos players_positions board] attempts to place a wall on the [board] at [pos] 
    without blocking any player in [players_positions]. 
    @param pos is the position where the wall is to be placed.
    @param players_positions is the array containing the positions of all players.
    @param board represents the current state of the game board.
    @return a new board state with the wall placed if the placement is valid.
    @raise [OutOfBounds] if the position is outside the board boundaries.
    @raise [InvalidWallPosition] if the given position is not a wall position.
    @raise [InvalidWallPlacement] if wall placement is invalid or blocks a player's path.
*)

val winning_player :
  Types.game -> Types.player
  (** [winning_player game] returns the first player of the game
    who is in an enemy zone i.e. the game is finished 
    @param game is the current game
    @return the player who won the game
    @raise Not_found otherwise *)