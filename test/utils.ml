open Quoridor.Types
open Quoridor.Board
open Quoridor.Engine

(** @raises Invalid_argument if the two lists are determined to have different lengths *)
let mapi2 f a b =
  if List.length a <> List.length b then
    raise (Invalid_argument "The two lists have different lengths.")
  else
    let rec aux i a b =
      match (a, b) with
      | [], [] -> []
      | ha :: ta, hb :: tb -> f i ha hb :: aux (i + 1) ta tb
      | _ -> raise (Invalid_argument "The two lists have different lengths.")
      (* Unreachable *)
    in
    aux 0 a b

let pp_color ff color =
  let color =
    match color with
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
    | Yellow -> "Yellow"
  in
  Format.fprintf ff "%s" color

let color_generator =
  let open QCheck in
  Gen.oneof
    [ Gen.return Red; Gen.return Green; Gen.return Yellow; Gen.return Blue ]

let arbitrary_color =
  QCheck.make ~print:(Format.asprintf "%a" pp_color) color_generator

let create_list_of_player nb_players walls_left strat =
  let colors = [ Red; Blue; Green; Yellow ] in
  let positions =
    [
      (board_size / 2, 0);
      (board_size / 2, board_size - 1);
      (0, board_size / 2);
      (board_size - 1, board_size / 2);
    ]
  in
  if nb_players <> 2 && nb_players <> 4 then
    raise
      (InvalidNumberPlayer
         (nb_players, "Number of players must be 2 or 4 to start the game"))
  else
    List.init nb_players (fun i ->
        create_player (List.nth positions i) walls_left (List.nth colors i)
          strat)

module Strategy = struct
  let random_move pos =
    let lstMv = list_of_moves pos in
    match lstMv with
    | [] ->
        raise (NoMovePossible "There is no movement possible for this player")
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
        validate_wall_placement (current_player ()) (x1, y1) (x1 + xv, y1 + yv);
        ((x1, y1), (x1 + xv, y1 + yv))
      with
      | InvalidWallPosition _ -> generate_random_wall_pos ()
      | InvalidPosition _ -> generate_random_wall_pos ()
      | InvalidWallPlacement _ -> generate_random_wall_pos ()
    in
    let wall_pos1, wall_pos2 = generate_random_wall_pos () in
    Placing_wall (wall_pos1, wall_pos2)

  let det_move pos =
    let r = Random.int 3 in
    if r == 0 && (current_player ()).walls_left > 0 then pos_wall_random ()
    else random_move pos
end
