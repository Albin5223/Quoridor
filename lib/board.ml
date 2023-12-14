open Types

type cell_content = Empty | Wall | Player of player
type board = cell_content array array

type state = {
  mutable players : player list;
  mutable status : game_status;
  mutable walls_pos : position list;
}

let board_size = 17
let game_board = Array.make_matrix board_size board_size Empty
let move_vectors = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

(* Storage of the player list to avoid browsing the entire board, as well as the game status. *)
let game_state = { players = []; status = WaitingToStart; walls_pos = [] }

let validate_game_in_progress_status () =
  match game_state.status with
  | WaitingToStart -> raise (InvalidGameState "Game has not started")
  | Finished _ -> raise (InvalidGameState "Game has already finished")
  | InProgress -> ()

let validate_game_waiting_status () =
  match game_state.status with
  | WaitingToStart -> ()
  | Finished _ -> raise (InvalidGameState "Game has already finished")
  | InProgress -> raise (InvalidGameState "Game has already started")

let reset_board () =
  for y = 0 to board_size - 1 do
    for x = 0 to board_size - 1 do
      game_board.(y).(x) <- Empty
    done
  done;
  game_state.players <- [];
  game_state.status <- WaitingToStart

let start_game () =
  let num_players = List.length game_state.players in
  if game_state.status = WaitingToStart then
    if num_players = 2 || num_players = 4 then
      if
        not
          (List.for_all
             (fun p -> p.walls_left = 20 / num_players)
             game_state.players)
      then
        raise
          (InvalidPlayerWallsLeft
             "The number of walls for each player is not allowed")
      else game_state.status <- InProgress
    else
      raise
        (InvalidNumberPlayer
           (num_players, "Number of players must be 2 or 4 to start the game"))
  else raise (InvalidGameState "Game cannot be started")

let stop_game winner =
  validate_game_in_progress_status ();
  game_state.status <- Finished winner

let current_player () =
  match game_state.players with
  | [] -> raise NoPlayersInGame
  | first :: _ -> first

let update_player_order () =
  match game_state.players with
  | [] -> ()
  | first :: rest -> game_state.players <- rest @ [ first ]

let validate_position pos =
  let x, y = pos in
  if not (x >= 0 && x < board_size && y >= 0 && y < board_size) then
    raise (InvalidPosition (pos, "Position is outside the board boundaries"))

let is_wall_position pos =
  validate_position pos;

  let x, y = pos in
  (x mod 2 = 1 && y mod 2 = 0) || y mod 2 = 1

let is_player_position pos =
  validate_position pos;

  let x, y = pos in
  x mod 2 = 0 && y mod 2 = 0

let get_cell_content pos =
  validate_position pos;

  let x, y = pos in
  game_board.(y).(x)

let is_wall pos = match get_cell_content pos with Wall -> true | _ -> false

let is_player pos =
  match get_cell_content pos with Player _ -> true | _ -> false

let is_wall_between pos1 pos2 =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if pos1 = pos2 then
    raise (InvalidPositionPair (pos1, pos2, "The two positions are the same"));

  if not (is_player_position pos1 && is_player_position pos2) then
    raise
      (InvalidPositionPair (pos1, pos2, "Positions must be even coordinates"));

  (* Checking the adjacency of the two provided positions *)
  if (abs (x1 - x2) = 2 && y1 = y2) || (x1 = x2 && abs (y1 - y2) = 2) then
    (* Calculation of the potential wall position *)
    let wall_pos = ((x1 + x2) / 2, (y1 + y2) / 2) in
    is_wall wall_pos
  else
    raise (InvalidPositionPair (pos1, pos2, "The positions are not adjacent"))

let adjacent_walls pos =
  let x, y = pos in

  if not (is_player_position pos) then
    raise
      (InvalidPlayerPosition (pos, "Given position is not a player's position"));

  List.fold_left
    (fun acc (dx, dy) ->
      (* We are on an even position, so the walls are on odd coordinates, thus +1 around us *)
      let newPos = (x + dx, y + dy) in
      try if is_wall newPos then newPos :: acc else acc
      with InvalidPosition _ -> acc)
    [] move_vectors

let adjacent_players pos =
  let x, y = pos in

  if not (is_player_position pos) then
    raise
      (InvalidPlayerPosition (pos, "Given position is not a player's position"));

  List.fold_left
    (fun acc (dx, dy) ->
      (* We are on an even position, so players are on even coordinates, thus +2 around us *)
      let newPos = (x + (2 * dx), y + (2 * dy)) in
      try if is_player newPos then newPos :: acc else acc
      with InvalidPosition _ -> acc)
    [] move_vectors

let list_of_moves pos =
  let x, y = pos in

  let wallsAround = adjacent_walls pos in
  let playersAround = adjacent_players pos in

  (* Checking for possible movements in each allowed direction *)
  List.fold_left
    (fun acc (dx, dy) ->
      let wallPos = (x + dx, y + dy) in
      let newPos = (x + (2 * dx), y + (2 * dy)) in

      (* Blocks the movement direction if there is a wall in the way. *)
      if List.exists (( = ) wallPos) wallsAround then acc
        (* Case where a player is present in the adjacent cell and we want to try to jump over them *)
      else if List.exists (( = ) newPos) playersAround then
        let jumpPos = (x + (4 * dx), y + (4 * dy)) in

        (* We can jump directly over the player in the same direction (no third player present) *)
        let valid_jump () =
          try (not (is_player jumpPos)) && not (is_wall_between newPos jumpPos)
          with InvalidPosition _ | InvalidPositionPair _ -> false
        in

        if valid_jump () then jumpPos :: acc
          (* Presence of a third player blocking the way *)
        else
          (* Adjacent cells to the player we are trying to jump over *)
          let adjacent_positions_around_newPos =
            List.map
              (fun (ddx, ddy) ->
                let newX, newY = newPos in
                (newX + (2 * ddx), newY + (2 * ddy)))
              move_vectors
          in

          (* Checking the valid adjacent cells to the blocking player *)
          let valid_adjacent_positions =
            List.fold_left
              (fun acc pos ->
                try
                  if
                    (not (is_player pos)) && not (is_wall_between newPos pos)
                    (* no player on the returned cell, nor a wall between this cell and the cell of the blocking player *)
                  then pos :: acc
                  else acc
                with InvalidPosition _ | InvalidPositionPair _ -> acc)
              [] adjacent_positions_around_newPos
          in
          List.append acc valid_adjacent_positions
      else
        (* Adds the move to the list if there is no wall or player blocking the direction. *)
        try if not (is_player newPos) then newPos :: acc else acc
        with InvalidPosition _ -> acc)
    [] move_vectors

let target_zone player =
  match player.start_position with
  | x, _ when x = 0 ->
      (* Start at left border *)
      fun (x, _) -> x = board_size - 1
  | x, _ when x = board_size - 1 ->
      (* Start at right border *)
      fun (x, _) -> x = 0
  | _, y when y = 0 ->
      (* Start at top border *)
      fun (_, y) -> y = board_size - 1
  | _, y when y = board_size - 1 ->
      (* Start at bottom border *)
      fun (_, y) -> y = 0
  | _ ->
      raise
        (InvalidPlayerPosition (player.start_position, "Invalid start position"))

let dfs_path_exists player pos1 pos2 =
  let start_pos = player.current_position in

  if not (is_player_position start_pos) then
    raise
      (InvalidPlayerPosition
         (start_pos, "Start position is not a player's position"));

  (* Creation of a matrix to track visited positions *)
  let visited = Array.make_matrix board_size board_size false in

  let rec dfs pos =
    let x, y = pos in
    let is_target_pos = target_zone player in
    if is_target_pos pos then true
    else if visited.(y).(x) then false
    else (
      visited.(y).(x) <- true;
      let next_moves = list_of_moves pos in

      List.exists
        (fun next_pos ->
          let next_x, next_y = next_pos in

          (* Excludes movements to positions pos1 and pos2 *)
          if next_pos = pos1 || next_pos = pos2 then false
          else if not visited.(next_y).(next_x) then dfs next_pos
          else false)
        next_moves)
  in
  dfs start_pos

let will_wall_block_player () =
  List.exists
    (fun player ->
      let pos = player.current_position in
      List.for_all
        (fun pos_vect ->
          Format.printf "pos: %d,%d"
            (fst pos + fst pos_vect)
            (snd pos + snd pos_vect);
          try is_wall (fst pos + fst pos_vect, snd pos + snd pos_vect)
          with InvalidPosition _ -> false)
        move_vectors)
    game_state.players

let validate_wall_placement player pos1 pos2 =
  if player.walls_left <= 0 then
    raise
      (InvalidWallPlacement (pos1, pos2, "Player has no walls left to place"));

  if not (is_wall_position pos1 && is_wall_position pos2) then
    raise
      (InvalidWallPosition (pos1, pos2, "Given position is not a wall position"));

  if is_wall pos1 || is_wall pos2 then
    raise
      (InvalidWallPlacement
         (pos1, pos2, "A wall already exists at this position"));

  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if not ((abs (x1 - x2) = 1 && y1 = y2) || (x1 = x2 && abs (y1 - y2) = 1)) then
    raise
      (InvalidWallPosition
         (pos1, pos2, "Wall positions must be adjacent and aligned"));

  if
    List.exists
      (fun player -> not (dfs_path_exists player pos1 pos2))
      game_state.players
  then
    raise
      (InvalidWallPlacement
         (pos1, pos2, "Wall placement blocks a player's path to goal"))

let compare_player p1 p2 = p1.color = p2.color

let place_wall pos1 pos2 =
  validate_game_in_progress_status ();

  let player = current_player () in
  validate_wall_placement player pos1 pos2;

  let x1, y1 = pos1 in
  let x2, y2 = pos2 in
  game_board.(y1).(x1) <- Wall;
  game_board.(y2).(x2) <- Wall;

  (* First update the player in the list, then in the game_board *)
  let updated_player = { player with walls_left = player.walls_left - 1 } in
  game_state.players <-
    List.map
      (fun p -> if compare_player p player then updated_player else p)
      game_state.players;

  let x, y = player.current_position in
  game_board.(y).(x) <- Player updated_player;

  if will_wall_block_player () then
    raise
      (InvalidWallPlacement
         (pos1, pos2, "Wall placement blocks a player's path to goal"));
  game_state.walls_pos <- pos1 :: pos2 :: game_state.walls_pos;
  update_player_order ()

let move_player pos =
  validate_game_in_progress_status ();

  let current_pos = (current_player ()).current_position in
  if not (List.exists (( = ) pos) (list_of_moves current_pos)) then
    raise
      (InvalidMove
         "The target position is not reachable from the current position")
  else
    let x, y = current_pos in
    game_board.(y).(x) <- Empty;

    (* First update the player in the list, then in the game_board *)
    let updated_player = { (current_player ()) with current_position = pos } in
    game_state.players <-
      List.map
        (fun p ->
          if compare_player p (current_player ()) then updated_player else p)
        game_state.players;

    let new_x, new_y = pos in
    game_board.(new_y).(new_x) <- Player updated_player;

    update_player_order ()

let is_border_position pos =
  validate_position pos;
  let x, y = pos in
  let middle = board_size / 2 in
  (x = middle && (y = 0 || y = board_size - 1))
  || (y = middle && (x = 0 || x = board_size - 1))

let add_player_to_board player =
  validate_game_waiting_status ();
  let current_players = game_state.players in
  let nbPlayers = List.length current_players in
  if nbPlayers >= 4 then
    raise
      (InvalidNumberPlayer
         (nbPlayers, "Cannot have more than 4 players on the board"));

  if player.current_position <> player.start_position then
    raise
      (InvalidPlayerPosition
         ( player.current_position,
           "Player must be placed on their start position" ));

  if List.exists (fun p -> p.color = player.color) current_players then
    raise
      (InvalidPlayerColor
         (player.color, "A player with the same color already exists"));

  let x, y = player.current_position in
  if not (is_border_position player.current_position) then
    raise
      (InvalidPlayerPosition
         (player.current_position, "Player must be placed on a border"));
  if is_player player.current_position then
    raise
      (InvalidPlayerPosition
         (player.current_position, "Player position is already occupied"));

  (* Adding the player to the list and to the game_board *)
  game_board.(y).(x) <- Player player;
  game_state.players <- game_state.players @ [ player ]

let add_all_players_to_board players =
  reset_board ();
  List.iter (fun p -> add_player_to_board p) players

let do_move (move : Types.move) =
  match move with
  | Placing_wall (pos1, pos2) -> place_wall pos1 pos2
  | Moving pos -> move_player pos

let play () =
  let strat = (current_player ()).strategy in
  let pos = (current_player ()).current_position in
  let move = strat pos in
  do_move move

let winning_player () =
  (* Function to check if a player has reached their target zone *)
  let player_reached_target player =
    (target_zone player) player.current_position
  in
  try
    let winner = List.find player_reached_target game_state.players in
    stop_game winner;
    winner
  with Not_found ->
    raise (NoWinningPlayer "No player has reached their target zone")

let print_player p =
  match p.color with
  | Red -> Format.printf " R "
  | Green -> Format.printf " G "
  | Blue -> Format.printf " B "
  | Yellow -> Format.printf " Y "

let print_cell cell =
  match cell with
  | Player p -> print_player p
  | Wall -> Format.printf " # "
  | Empty -> Format.printf " . "

let print_board () =
  let print_row row = Array.iter (fun cell -> print_cell cell) row in
  Format.printf "@.";
  Array.iter
    (fun row ->
      print_row row;
      Format.printf "@;")
    game_board

let color_to_string player =
  match player.color with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  | Yellow -> "yellow"

let get_all_wall_pos () = List.map (fun (x, y) -> (x, y)) game_state.walls_pos

let get_all_player_pos () =
  List.map (fun p -> (p.start_position, p.current_position)) game_state.players

let get_color pos =
  let x, y = pos in
  let cell = game_board.(y).(x) in
  match cell with
  | Player p -> p.color
  | Wall -> raise (InvalidPosition (pos, "Wall position"))
  | Empty -> raise (InvalidPosition (pos, "Empty position"))
