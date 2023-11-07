(** engine.ml
    Implementation of the game's engine; handling initialization, state transitions,
    player actions, and victory conditions.
*)

open Board
open Types

(** {1 Initialisation Functions} *)

(** [init_board] initializes a game board with all cells set to Empty.
    @return a board_size x board_size array with all cells set to Empty.
*)
let init_board =
  let board = Array.make_matrix board_size board_size Empty in
  board

(** [init_player pos color] initializes a player with a given position and color.
    @param pos the starting position of the player.
    @param color the color assigned to the player.
    @return a player record with the given position, 10 walls left, and specified color.
*)
let init_player pos color = { position = pos; walls_left = 10; color }

(** [add_players_to_board board players] adds players to the board.
    @param board the game board to add players to.
    @param players the list of players to be added to the board.
    @return a new board with players added to their starting positions.
*)
let add_players_to_board board players =
  let newBoard = Array.map Array.copy board in
  let add_player_aux p =
    let x, y = p.position in
    newBoard.(y).(x) <- Player p
  in
  List.iter add_player_aux players;
  newBoard

(** [init_game nb_players] initializes the game with a specified number of players.
    @param nb_players the number of players (must be between 2 and 4 inclusive).
    @return the initial state of the game with players placed on the board and the first player set as the current player.
    @raise InvalidNumberPlayer if nb_players is not between 2 and 4.
*)
let init_game nb_players =
  let player_positions =
    match nb_players with
    | 2 -> [| (board_size / 2, 0); (board_size / 2, board_size - 1) |]
    | 3 ->
        [|
          (board_size / 2, 0);
          (board_size / 2, board_size - 1);
          (0, board_size / 2);
        |]
    | 4 ->
        [|
          (board_size / 2, 0);
          (board_size / 2, board_size - 1);
          (0, board_size / 2);
          (board_size - 1, board_size / 2);
        |]
    | _ ->
        raise
          (InvalidNumberPlayer
             "the number of players must be between 2 and 4 inclusive")
  in
  let color_list = [| Red; Blue; Green; Yellow |] in
  let players_list =
    List.init nb_players (fun i ->
        init_player player_positions.(i) color_list.(i))
  in
  let board = add_players_to_board init_board players_list in
  let current_player = List.nth players_list 0 in
  {
    players = players_list;
    board;
    current_player;
    state = Ingame;
    winner = None;
  }

(** {1 Game State Update Functions} *)

(** [change_pos_of_player game player pos] changes the position of a player on the board.
    @param game the current state of the game.
    @param player the player whose position is to be changed.
    @param pos the new position for the player.
    @return a new game state with the player moved to the new position.
    @raise Invalid_argument if the old or new position is out of bounds.
*)
let change_pos_of_player game player pos =
  let newBoard = Array.map Array.copy game.board in
  let x, y = pos in
  let x_old, y_old = player.position in
  if not (is_valid_position (x_old, y_old)) then
    raise (Invalid_argument "Old position is out of bounds");
  if not (is_valid_position (x, y)) then
    raise (Invalid_argument "New position is out of bounds");
  newBoard.(y).(x) <- Player player;
  newBoard.(y_old).(x_old) <- Empty;
  let new_player = { player with position = pos } in
  let new_lst_players =
    new_player :: List.filter (fun pl -> pl <> player) game.players
  in

  {
    game with
    players = new_lst_players;
    board = newBoard;
    current_player = new_player;
  }

(** [move game player] moves a player to a new position chosen at random from the list of valid moves.
    @param game the current state of the game.
    @param player the player to move.
    @return a new game state with the player moved to the new position.
*)
let move game player =
  let lstMv = list_of_moves player.position game.board in
  match lstMv with
  | [] -> game
  | _ ->
      let r = Random.int (List.length lstMv) in
      let newPos = List.nth lstMv r in
      change_pos_of_player game player newPos

(** [place_wall_random game player] attempts to place a wall at a random position.
    @param game the current state of the game.
    @param player the player attempting to place a wall.
    @return a new game state with the wall placed if successful.
    @raise InvalidWallPlacement if the wall placement is not possible.
*)
let rec place_wall_random game player =
  let rec generate_random_wall_pos () =
    let x1 = Random.int board_size in
    let y1 = Random.int board_size in
    let r = Random.int 4 in
    let xv, yv = List.nth move_vectors r in
    Format.printf "%d,%d " x1 y1;
    try
      if is_wall_position (x1, y1) && is_wall_position (x1 + xv, y1 + yv) then
        ((x1, y1), (x1 + xv, y1 + yv))
      else generate_random_wall_pos ()
    with InvalidPosition _ -> generate_random_wall_pos ()
  in
  let wall_pos1, wall_pos2 = generate_random_wall_pos () in
  Format.printf "\n\nPlacing wall on pos1 (%d, %d) & pos2 (%d, %d)\n\n"
    (fst wall_pos1) (snd wall_pos1) (fst wall_pos2) (snd wall_pos2);

  try
    let new_board = place_wall wall_pos1 wall_pos2 game.players game.board in
    let new_player = { player with walls_left = player.walls_left - 1 } in
    let new_lst_players =
      new_player :: List.filter (fun pl -> pl <> player) game.players
    in
    {
      game with
      players = new_lst_players;
      board = new_board;
      current_player = new_player;
    }
  with InvalidWallPlacement _ -> place_wall_random game player

(** [det_move game player] determines and executes the next action for a player.
    @param game the current state of the game.
    @param player the player whose action is to be determined.
    @return a new game state after the player's action.
*)
let det_move game player =
  let r = Random.int 3 in
  if r == 0 && player.walls_left > 0 then place_wall_random game player
  else move game player

(** [change_current_player game] updates the game state to set the next player as the current player.
    @param game the current state of the game.
    @return a new game state with the next player set as the current player.
*)
let change_current_player game =
  let current_player = game.current_player in
  let new_player_lst =
    List.filter (fun p -> p <> current_player) game.players @ [ current_player ]
  in
  {
    game with
    players = new_player_lst;
    current_player = List.nth new_player_lst 0;
  }

(** {1 Winning Condition Functions} *)

(** [winning_player game] determines if there is a winning player who has reached their target zone.
    @param game the current state of the game.
    @return the player who has won the game.
    @raise NoWinningPlayer if no player has reached their target zone.
*)
let winning_player game =
  (* Hashtable to associate colors with the conditions to check if they are in their target zones *)
  let colors_zones = Hashtbl.create 4 in
  Hashtbl.add colors_zones Red (fun _ y -> y = board_size - 1);
  Hashtbl.add colors_zones Green (fun x _ -> x = board_size - 1);
  Hashtbl.add colors_zones Blue (fun _ y -> y = 0);
  Hashtbl.add colors_zones Yellow (fun x _ -> x = 0);
  (* Function to check if a player has reached their target zone *)
  let player_reached_target player =
    Hashtbl.fold
      (fun k v acc ->
        acc
        ||
        if k = player.color then v (fst player.position) (snd player.position)
        else false)
      colors_zones false
    (* Find the player who has reached their target zone.
       Raises Not_found with a descriptive error message if no player is found *)
  in

  try List.find player_reached_target game.players
  with Not_found ->
    raise (NoWinningPlayer "No player has reached their target zone")

(** {1 Game Execution Function} *)

(** [run_game] starts and manages the game loop until a player wins or the game ends.
*)
let run_game =
  Random.self_init ();
  let rec aux game =
    print_board game.board;
    if game.state = Ingame then
      let game = det_move game game.current_player in
      let game = change_current_player game in
      try
        let winner = winning_player game in
        aux
          {
            game with
            state = GameOver game.current_player;
            winner = Some winner;
          }
      with NoWinningPlayer _ -> aux game
    else Format.printf "partie terminée\n"
  in
  aux (init_game 4)
