open Board
open Types


let init_board = 
  let board = Array.make_matrix board_size board_size Empty in 
    board

let init_player pos color = {
  position = pos; 
  walls_left = 10; 
  color = color;}

let add_players_to_board board players = 
  let newBoard = Array.map Array.copy board in
    let add_player_aux p = 
      let (x,y) = p.position in 
        newBoard.(y).(x) <- Player p 
      in
      List.iter add_player_aux players;
      newBoard
   
let init_game nb_players = let player_positions =
  match nb_players with
    | 2 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2)|]
    | 3 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2); (board_size / 2, 0)|]
    | 4 ->
      [|(0, board_size / 2); (board_size - 1, board_size / 2); (board_size / 2, 0); (board_size / 2, board_size - 1)|]
    | _ -> raise (InvalidNumberPlayer "the number of players must be between 2 and 4 inclusive")
    in let color_list = [|Blue; Red; Yellow; Green|] in 
      let players_list = List.init nb_players (fun i -> init_player player_positions.(i) color_list.(i)) in
      let board = add_players_to_board init_board players_list in 
      let current_player = List.nth players_list 0 in
      { players = players_list; board = board; current_player = current_player; state = Ingame; }

let change_pos_of_player game player pos =
  let newBoard = Array.map Array.copy game.board in
  let x, y = pos in
  let x_old, y_old = player.position in
  newBoard.(y_old).(x_old) <- Empty;
  newBoard.(y).(x) <- Player player;
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
      change_pos_of_player game player newPos


let rec place_wall_random game player =
  let rec generate_random_wall_pos () =
    let x1 = Random.int board_size in
    let y1 = Random.int board_size in
    let r = Random.int 4 in let (xv,yv) = move_vectors.(r) in
    Format.printf "%d,%d " x1 y1 ; 
    if is_wall_position (x1, y1) && is_wall_position (x1+xv,y1+yv) then ((x1, y1),(x1+xv,y1+yv))
    else generate_random_wall_pos ()
  in
  let (wall_pos1,wall_pos2) = generate_random_wall_pos () in 
  try { game with board = place_wall wall_pos1 wall_pos2 game.players game.board }
  with InvalidWallPlacement _ -> place_wall_random game player

let det_move game player = 
  let r = Random.int 2 in
  if r == 0 then move game player else place_wall_random game player

(* TODO : verify that code is running correctly, add robustness and complets it *)

let change_current_player game = let current_player = game.current_player in
  let new_player_lst = (List.filter (fun p -> p <> current_player) game.players) @ [current_player] in
    { game with 
      players = new_player_lst;
      current_player = List.nth new_player_lst 0;
    }

(*fonction non terminée (en attente de la fonction de verification de victoire)*)
let run_game = Random.self_init ();
  let rec aux game = 
    print_board game.board;
    if game.state = Ingame then 
      let game = det_move game game.current_player in
        let game = change_current_player game in let r = Random.int 10 in
          Format.printf "%d" r;
          if r == 0 then aux {game with state = (GameOver game.current_player)}
          else aux game
    else Format.printf "partie terminée" 
      in aux (init_game 2)

