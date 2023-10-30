open Board
open Types

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
