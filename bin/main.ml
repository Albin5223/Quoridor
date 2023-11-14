open Quoridor.Engine


let random_move () =
  let lstMv = Board.list_of_moves (pos_current_player ()) in
  match lstMv with
  | [] -> ()
  | _ ->
      let r = Random.int (List.length lstMv) in
      let newPos = List.nth lstMv r in
      Board.move_player newPos

let rec place_wall_random () =
  let rec generate_random_wall_pos () =
    let x1 = Random.int Board.board_size in
    let y1 = Random.int Board.board_size in
    let r = Random.int 4 in
    let xv, yv = List.nth Board.move_vectors r in
    Format.printf "%d,%d " x1 y1;
    try
      if
        Board.is_wall_position (x1, y1)
        && Board.is_wall_position (x1 + xv, y1 + yv)
      then ((x1, y1), (x1 + xv, y1 + yv))
      else generate_random_wall_pos ()
    with InvalidPosition _ -> generate_random_wall_pos ()
  in
  let wall_pos1, wall_pos2 = generate_random_wall_pos () in
    try Board.place_wall wall_pos1 wall_pos2
    with InvalidWallPlacement _ -> place_wall_random ()

let det_move () =
  let r = Random.int 3 in
  if r == 0 && walls_left_current_player () > 0 then place_wall_random
  else random_move


let () =
  let _ = run_game () in
  ()
