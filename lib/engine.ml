open Types
open Board

let init_game nb_players =
  (* Détermine les positions initiales des joueurs en fonction du nombre de joueurs *)
  let player_positions =
    match nb_players with
    | 2 ->
        [|
          (Board.board_size / 2, 0); (Board.board_size / 2, Board.board_size - 1);
        |]
    | 3 ->
        [|
          (Board.board_size / 2, 0);
          (Board.board_size / 2, Board.board_size - 1);
          (0, Board.board_size / 2);
        |]
    | 4 ->
        [|
          (Board.board_size / 2, 0);
          (Board.board_size / 2, Board.board_size - 1);
          (0, Board.board_size / 2);
          (Board.board_size - 1, Board.board_size / 2);
        |]
    | _ ->
        raise
          (InvalidNumberPlayer
             ( nb_players,
               "The number of players must be between 2 and 4 inclusive" ))
  in
  (* Création de la liste des joueurs avec leurs couleurs et positions initiales *)
  let color_list = [| Red; Blue; Green; Yellow |] in
  (* Création et ajout des joueurs au jeu *)
  Array.iteri
    (fun i pos ->
      let color = color_list.(i) in
      Board.add_player_to_board color pos)
    player_positions

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
  Format.printf "\n\nPlacing wall on pos1 (%d, %d) & pos2 (%d, %d)\n\n"
    (fst wall_pos1) (snd wall_pos1) (fst wall_pos2) (snd wall_pos2);

  try Board.place_wall wall_pos1 wall_pos2
  with InvalidWallPlacement _ -> place_wall_random ()

let det_move () =
  let r = Random.int 3 in
  if r == 0 && walls_left_current_player () > 0 then place_wall_random
  else random_move

let run_game () =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let _ = Board.winning_player () in
      Format.printf "partie terminée\n"
    with NoWinningPlayer _ ->
      det_move () ();
      aux ()
  in
  init_game 4;
  aux ()

(*let run_game () =
    Random.self_init ();
    let rec aux i max =
      if i >= max then () else
      Board.print_board ();
      try
        let _ = Board.winning_player () in
        Format.printf "partie terminée\n"
      with
      | NoWinningPlayer _ ->
          det_move () ();
          aux (i+1) max
    in
  init_game 4;
  aux 0 5*)
