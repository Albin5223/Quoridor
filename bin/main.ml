open Quoridor.Engine
open Quoridor.Board
open Quoridor.Types

let random_move pos =
  let lstMv = list_of_moves pos in
  match lstMv with
  | [] -> raise (NoMove "There is no movement possible for this player")
  | _ ->
      let r = Random.int (List.length lstMv) in
      let newPos = List.nth lstMv r in
      Moving newPos

let pos_wall_random () =
  let rec generate_random_wall_pos () =
    let x1 = Random.int board_size in
    let y1 = Random.int board_size in
    let r = Random.int 4 in
    let xv, yv = List.nth move_vectors r in
    try
      validate_wall_placement
        (walls_left_current_player ())
        (x1, y1)
        (x1 + xv, y1 + yv);
      ((x1, y1), (x1 + xv, y1 + yv))
    with
    | InvalidWallPosition _ -> generate_random_wall_pos ()
    | InvalidPosition _ -> generate_random_wall_pos ()
    | InvalidWallPlacement _ -> generate_random_wall_pos ()
  in
  let wall_pos1, wall_pos2 = generate_random_wall_pos () in
  Wall (wall_pos1, wall_pos2)

let det_move pos =
  let r = Random.int 3 in
  if r == 0 && walls_left_current_player () > 0 then pos_wall_random ()
  else random_move pos

let create_lst_of_attributs () =
  let colors = [ Red; Blue; Green; Yellow ] in
  let positions =
    [
      (board_size / 2, 0);
      (board_size / 2, board_size - 1);
      (0, board_size / 2);
      (board_size - 1, board_size / 2);
    ]
  in
  List.init 4 (fun i -> (List.nth colors i, List.nth positions i, det_move))

let () =
  let lst_attribut = create_lst_of_attributs () in
  let _ = run_game lst_attribut in
  ()
