open Types
(** Specification of the structure of the game board and its characteristics; 
    functions related to movement verification, display, etc. *)

(** The size of the board in both length and width. *)
let board_size = 17

(** [is_valid_position pos] checks if the given position [pos] is valid on the game board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is within the bounds of the board, false otherwise.
*)
let is_valid_position pos =
  let x, y = pos in
  x >= 0 && x < board_size && y >= 0 && y < board_size

(** [get_cell_content pos board] retrieves the content of the cell at the given [pos] on the [board].
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @param board is the 2D array representing the game board.
    @return the content of the cell at the specified position.
    @raise raises OutOfBounds if the provided position is not valid on the board.
*)
let get_cell_content pos board =
  if not (is_valid_position pos) then
    raise (OutOfBounds "Position is outside the board boundaries");
  let x, y = pos in
  board.(y).(x)

(** [is_wall cell] determines if a given [cell] represents a wall on the board.
    @return true if the cell is a wall, otherwise false.
*)
let is_wall = function Wall -> true | _ -> false

(** [is_player cell] determines if a given [cell] represents a player on the board.
    @return true if the cell is a player, otherwise false.
*)
let is_player = function Player _ -> true | _ -> false

(** [is_wall_position pos] determines if a given position [pos] is valid for a wall on the board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is valid for a wall, false otherwise.
    @raise InvalidPosition if the position is not valid on the board.
*)
let is_wall_position pos =
  if not (is_valid_position pos) then
    raise (InvalidPosition "Position is not valid on the board");

  let x, y = pos in
  (x mod 2 = 1 && y mod 2 = 0) || y mod 2 = 1

(** [is_player_position pos] determines if a given position [pos] is valid for a player on the board.
    @param pos is a tuple representing the (x, y) coordinates on the board.
    @return true if the position is valid for a player, false otherwise.
    @raise InvalidPosition if the position is not valid on the board.
*)
let is_player_position pos =
  if not (is_valid_position pos) then
    raise (InvalidPosition "Position is not valid on the board");

  let x, y = pos in
  x mod 2 = 0 && y mod 2 = 0

(** List defining the potential movement vectors from a position *)
let move_vectors =
  [
    (-1, 0);
    (* Move to the left *)
    (1, 0);
    (* Move to the right *)
    (0, -1);
    (* Move upwards *)
    (0, 1) (* Move downwards *);
  ]

(** [list_of_walls pos board] produces a list of wall positions adjacent to a given position.
    @param pos is the starting position for which we want to find adjacent walls.
    @param board is the current game board.
    @return a list containing positions (coordinates) of adjacent walls.
    @raise OutOfBounds if the provided position is outside the board boundaries.
    @raise InvalidPlayerPosition if the provided position itself is not a player position.
*)
let list_of_walls pos board =
  let x, y = pos in

  if not (is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

  (* Iterate over possible directions, accumulating valid wall positions into a list *)
  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + dx, y + dy) in
      if is_valid_position newPos && is_wall (get_cell_content newPos board)
      then newPos :: acc
      else acc)
    [] move_vectors

(** [list_of_players pos board] returns a list of positions of players 
    around the specified position [pos] on the [board], considering direct 
    neighbors two steps away in the vertical and horizontal directions.
    @param pos is the position around which we are searching for players.
    @param board is the current game board.
    @raise OutOfBounds if the given position is outside of the board boundaries.
    @raise InvalidPlayerPosition if the given position is not a player position.
*)
let list_of_players pos board =
  let x, y = pos in

  if not (is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + (2 * dx), y + (2 * dy)) in
      if is_valid_position newPos && is_player (get_cell_content newPos board)
      then newPos :: acc
      else acc)
    [] move_vectors

(** [is_wall_between pos1 pos2 board] checks if there's a wall between 
    two positions [pos1] and [pos2] on the [board].
    @param pos1 is the first position to check.
    @param pos2 is the second position to check.
    @param board is the current game board.
    @raise OutOfBounds if either of the positions is outside the board boundaries.
    @raise InvalidPosition if positions are not even coordinates.
*)
let is_wall_between pos1 pos2 board =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if pos1 = pos2 then raise (InvalidPosition "The two positions are the same");
  if not (is_valid_position pos1 && is_valid_position pos2) then
    raise (OutOfBounds "One of the positions is outside the board boundaries");
  if not (is_player_position pos1 && is_player_position pos2) then
    raise (InvalidPosition "Positions must be even coordinates");

  (* Check if the positions are adjacent *)
  if (abs (x1 - x2) = 2 && y1 = y2) || (x1 = x2 && abs (y1 - y2) = 2) then
    let wall_pos = ((x1 + x2) / 2, (y1 + y2) / 2) in
    is_wall_position wall_pos && is_wall (get_cell_content wall_pos board)
  else raise (InvalidPosition "The positions are not adjacent")

(** [list_of_moves pos board] calculates the possible moves for a player at 
    position [pos] on the [board]. The moves are determined based on the neighboring 
    walls, players, and empty cells.
    @param pos is the current position of the player on the board.
    @param board represents the current state of the game board.
    @return an array containing the possible move positions for the player.
    @raise [OutOfBounds] if the position is outside the board boundaries.
    @raise [InvalidPlayerPosition] if the position is not a player position.
*)
let list_of_moves pos board =
  let x, y = pos in

  (* Validate the position on the board *)
  if not (is_valid_position (x, y)) then
    raise (OutOfBounds "Position is outside the board boundaries");
  if not (is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

  (* Obtain walls and players adjacent to the current position *)
  let wallsAround = list_of_walls pos board in
  let playersAround = list_of_players pos board in

  (* Iterate over possible move vectors to determine the set of valid moves.
      This function accumulates a list of moves by examining each potential move
      relative to walls and other players.
  *)
  List.fold_left
    (fun acc (dx, dy) ->
      let wallPos = (x + dx, y + dy) in
      let newPos = (x + (2 * dx), y + (2 * dy)) in

      (* Block move direction if there's a wall in the path *)
      if List.exists (( = ) wallPos) wallsAround then acc
        (* Handle cases where there's a player in the adjacent cell *)
      else if List.exists (( = ) newPos) playersAround then
        let jumpPos = (x + (4 * dx), y + (4 * dy)) in
        if
          is_valid_position jumpPos
          && (not (is_player (get_cell_content jumpPos board)))
          && not (is_wall_between newPos jumpPos board)
        then jumpPos :: acc
        else
          (* Check for valid moves around the obstructing player *)
          let adjacent_positions_around_newPos =
            List.map
              (fun (ddx, ddy) ->
                let newX, newY = newPos in
                (* position of the obstructing player *)
                (newX + (2 * ddx), newY + (2 * ddy)))
              move_vectors
          in
          let valid_adjacent_positions =
            List.fold_left
              (fun acc pos ->
                if
                  is_valid_position pos
                  && (not (is_player (get_cell_content pos board)))
                  && not (is_wall_between newPos pos board)
                then pos :: acc
                else acc)
              [] adjacent_positions_around_newPos
          in
          List.append acc valid_adjacent_positions
        (* Add move direction if there's an unoccupied cell *)
      else if
        is_valid_position newPos
        && not (is_player (get_cell_content newPos board))
      then newPos :: acc
      else acc)
    [] move_vectors

(** [dfs_path_exists start_pos board] determines if there's a path from [start_pos] 
    to the target position on the [board] using a depth-first search (DFS). 
    @param start_pos is the starting position for the DFS.
    @param board is the current game board.
    @return [true] if a path exists, [false] otherwise.
    @raise [OutOfBounds] if the start position is outside the board boundaries.
    @raise [InvalidPlayerPosition] if the start position is not a player position.
*)
let dfs_path_exists start_pos board =
  (* Validate the start position *)
  if not (is_valid_position start_pos) then
    raise (OutOfBounds "Start position is outside the board boundaries");
  if not (is_player_position start_pos) then
    raise (InvalidPlayerPosition "Start position is not a player's position");

  (* Create a matrix to keep track of visited positions *)
  let visited = Array.make_matrix board_size board_size false in

  (* [is_target_position pos] determines if the given position [pos] is a target position
      based on the player's color.
      @param pos is the position to be checked.
      @return [true] if it's a target position, [false] otherwise.
  *)
  let is_target_position pos =
    let x, y = pos in
    match get_cell_content pos board with
    | Player { color = Blue; _ } -> y = 0
    | Player { color = Red; _ } -> y = board_size - 1
    | Player { color = Yellow; _ } -> x = 0
    | Player { color = Green; _ } -> x = board_size - 1
    | _ -> false
  in

  (* [dfs pos] is a recursive function that performs the depth-first search
      starting from the given position [pos].
      @param pos is the current position during the DFS.
      @return [true] if a path to the target is found from [pos], [false] otherwise.
  *)
  let rec dfs pos =
    let x, y = pos in
    if is_target_position pos then true
    else if visited.(y).(x) then false
    else (
      visited.(y).(x) <- true;
      let next_moves = list_of_moves pos board in
      List.exists
        (fun next_pos ->
          (not visited.(snd next_pos).(fst next_pos)) && dfs next_pos)
        next_moves)
  in
  dfs start_pos

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
let can_place_wall pos players_positions board =
  (* Validate the position *)
  if not (is_valid_position pos) then
    raise (OutOfBounds "Position is outside the board boundaries");
  if not (is_wall_position pos) then
    raise (InvalidWallPosition "Given position is not a wall position");

  (* Check if there's already a wall at the given position *)
  if is_wall (get_cell_content pos board) then
    raise (InvalidWallPlacement "A wall already exists at this position");

  (* Create a temporary copy of the board to simulate placing the wall *)
  let temp_board = Array.map Array.copy board in
  let x, y = pos in

  (* [wall_inserted] checks and simulates the placement of a wall either
      vertically or horizontally based on the coordinates [x] and [y].
      @return [true] if the wall can be inserted at the given position, [false] otherwise.
  *)
  let wall_inserted =
    match (x mod 2, y mod 2) with
    | 1, 0 ->
        (* vertical wall *)
        if
          y < board_size - 2
          && is_valid_position (x, y + 1)
          && (not (is_wall (get_cell_content (x, y + 1) temp_board)))
          && is_valid_position (x, y + 2)
          && not (is_wall (get_cell_content (x, y + 2) temp_board))
        then (
          temp_board.(y).(x) <- Wall;
          temp_board.(y + 1).(x) <- Wall;
          temp_board.(y + 2).(x) <- Wall;
          true)
        else false
    | _, 1 ->
        (* horizontal wall *)
        if
          x < board_size - 2
          && is_valid_position (x + 1, y)
          && (not (is_wall (get_cell_content (x + 1, y) temp_board)))
          && is_valid_position (x + 2, y)
          && not (is_wall (get_cell_content (x + 2, y) temp_board))
        then (
          temp_board.(y).(x) <- Wall;
          temp_board.(y).(x + 1) <- Wall;
          temp_board.(y).(x + 2) <- Wall;
          true)
        else false
    | _ -> false
  in

  (* If the wall is simulated to be inserted, verify each player can reach their target *)
  if not wall_inserted then
    raise
      (InvalidWallPlacement
         "Wall placement is either invalid or overlaps with an existing wall");

  Array.for_all
    (fun player_pos -> dfs_path_exists player_pos temp_board)
    players_positions

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
let place_wall pos players_positions board =
  (* Validate the position *)
  if not (is_valid_position pos) then
    raise (OutOfBounds "Position is outside the board boundaries");
  if not (is_wall_position pos) then
    raise (InvalidWallPosition "Given position is not a wall position");

  (* Check if there's already a wall at the given position *)
  if is_wall (get_cell_content pos board) then
    raise (InvalidWallPlacement "A wall already exists at this position");

  (* Create a temporary copy of the board for simulating the wall placement *)
  let temp_board = Array.map Array.copy board in
  let x, y = pos in

  (* [modify_board_for_wall ()] is an inner function that adjusts the [temp_board]
      based on the wall placement logic, depending on the coordinates [x] and [y].
      @return [true] if the wall can be placed at the specified position, [false] otherwise.
  *)
  let modify_board_for_wall () =
    match (x mod 2, y mod 2) with
    | 1, 0 ->
        (* vertical wall *)
        if
          y < board_size - 2
          && is_valid_position (x, y + 1)
          && (not (is_wall (get_cell_content (x, y + 1) temp_board)))
          && is_valid_position (x, y + 2)
          && not (is_wall (get_cell_content (x, y + 2) temp_board))
        then (
          temp_board.(y).(x) <- Wall;
          temp_board.(y + 1).(x) <- Wall;
          temp_board.(y + 2).(x) <- Wall;
          true)
        else false
    | _, 1 ->
        (* horizontal wall *)
        if
          x < board_size - 2
          && is_valid_position (x + 1, y)
          && (not (is_wall (get_cell_content (x + 1, y) temp_board)))
          && is_valid_position (x + 2, y)
          && not (is_wall (get_cell_content (x + 2, y) temp_board))
        then (
          temp_board.(y).(x) <- Wall;
          temp_board.(y).(x + 1) <- Wall;
          temp_board.(y).(x + 2) <- Wall;
          true)
        else false
    | _ -> false
  in

  (* Validate wall placement and ensure no overlap with existing walls *)
  if not (modify_board_for_wall ()) then
    raise (InvalidWallPlacement "Invalid wall placement or overlap");

  (* Check if the wall placement still allows all players to achieve their goals *)
  if
    Array.for_all
      (fun player_pos -> dfs_path_exists player_pos temp_board)
      players_positions
  then temp_board
  else
    raise (InvalidWallPlacement "Wall placement blocks a player's path to goal")
