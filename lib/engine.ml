open Board
open Types
let init_board = 
  let board = Array.make_matrix board_size board_size Empty in 
    board

let pp_board = print_board init_board

let init_player pos color = {
  position = pos; 
  walls_left = 10; 
  color = color;}

let init_game nb_players = let player_positions =
  match nb_players with
    | 2 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2)|]
    | 3 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2); (board_size / 2, 0)|]
    | 4 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2); (board_size / 2, 0); (board_size / 2, board_size - 1)|]
    | _ -> raise (InvalidNumberPlayer "the number of players must be between 2 and 4 inclusive")
    in let color_list = [|Red; Green; Blue; Yellow|] in 
      let players_list = List.init nb_players (fun i -> init_player player_positions.(i) color_list.(i)) in
      let board = init_board in 
      let current_player = List.nth players_list 0 in
      { players = players_list; board = board; current_player = current_player; game_state = Ingame; }

let change_pos_of_player game pos =
  let newBoard = Array.map Array.copy game.board in
  let player = game.current_player in
  let x, y = pos in
  newBoard.(y).(x) <- Player player;
  let x_old, y_old = player.position in
  newBoard.(y_old).(x_old) <- Empty;
  {
    game with
    board = newBoard;
    current_player = { game.current_player with position = pos };
  }

let move game player =
  let lstMv = list_of_moves player.position game.board in
  match lstMv with
  | [||] -> game
  | _ ->
      let r = Random.int (Array.length lstMv) in
      let newPos = lstMv.(r) in
      change_pos_of_player game newPos

let rec place_wall_random game player =
  let rec generate_random_wall_pos () =
    let x = Random.int board_size in
    let y = Random.int board_size in
    if x mod 2 = 1 && y mod 2 = 1 && is_wall_position (x, y) then (x, y)
    else generate_random_wall_pos ()
  in
  let wall_pos = generate_random_wall_pos () in
  try { game with board = place_wall wall_pos [| player.position |] game.board }
  with InvalidWallPlacement _ -> place_wall_random game player

let det_move game player =
  let r = Random.int 2 in
  if r == 0 then move game player else place_wall_random game player

(* TODO : verify that code is running correctly, add robustness and complets it *)
