open Quoridor
open Quoridor.Types

module Strategy = struct
  open Quoridor.Types
  open Quoridor.Board

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
end

let test_add_player_valid =
  Alcotest.test_case "add_player_valid" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (0, Board.board_size / 2) Strategy.det_move;
      Alcotest.(check bool)
        "Player added correctly" true
        (Board.is_player (0, Board.board_size / 2)))

let test_add_player_invalid_position =
  Alcotest.test_case "add_player_invalid_position" `Quick (fun () ->
      Board.reset_board ();
      try
        Board.add_player_to_board Blue (1, 1) Strategy.det_move;
        Alcotest.fail "No exception raised for invalid position"
      with InvalidPlayerPosition _ -> ())

let test_move_player_valid =
  Alcotest.test_case "move_player_valid" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (8, 16) Strategy.det_move;
      Board.add_player_to_board Blue (8, 0) Strategy.det_move;
      Board.start_game ();
      Board.move_player (8, 14);
      Alcotest.(check bool)
        "Player moved correctly" true
        (Board.is_player (8, 14)))

let test_move_player_invalid =
  Alcotest.test_case "move_player_invalid" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (8, 16) Strategy.det_move;
      Board.add_player_to_board Blue (8, 0) Strategy.det_move;
      Board.start_game ();
      try
        Board.move_player (8, 13);
        Alcotest.fail "No exception raised for invalid move"
      with InvalidMove _ -> ())

let test_place_wall_valid =
  Alcotest.test_case "place_wall_valid" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (8, 16) Strategy.det_move;
      Board.add_player_to_board Blue (8, 0) Strategy.det_move;
      Board.start_game ();
      Board.place_wall (1, 0) (1, 1);
      Alcotest.(check bool)
        "Wall placed correctly" true
        (Board.is_wall (1, 0) && Board.is_wall (1, 1)))

let test_place_wall_invalid =
  Alcotest.test_case "place_wall_invalid" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (8, 16) Strategy.det_move;
      Board.add_player_to_board Blue (8, 0) Strategy.det_move;
      Board.start_game ();
      try
        Board.place_wall (1, 1) (1, 3);
        Alcotest.fail "No exception raised for invalid wall placement"
      with InvalidWallPosition _ -> ())

let test_winning_player_none =
  Alcotest.test_case "winning_player_none" `Quick (fun () ->
      Board.reset_board ();
      Board.add_player_to_board Red (0, Board.board_size / 2) Strategy.det_move;
      Board.add_player_to_board Yellow (8, 0) Strategy.det_move;
      try
        let _ = Board.winning_player () in
        Alcotest.fail "No exception raised for no winning player"
      with NoWinningPlayer _ -> ())

let test_validate_position_valid =
  Alcotest.test_case "validate_position_valid" `Quick (fun () ->
      try
        Board.validate_position (0, 0);
        ()
      with _ -> Alcotest.fail "Position validation failed unexpectedly")

let test_validate_position_invalid =
  Alcotest.test_case "validate_position_invalid" `Quick (fun () ->
      try
        Board.validate_position (-1, -1);
        Alcotest.fail "Invalid position not detected"
      with InvalidPosition _ -> ())

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ( "add_player_to_board",
        [ test_add_player_valid; test_add_player_invalid_position ] );
      ("move_player", [ test_move_player_valid; test_move_player_invalid ]);
      ("place_wall", [ test_place_wall_valid; test_place_wall_invalid ]);
      ("winning_player", [ test_winning_player_none ]);
      ( "validate_position",
        [ test_validate_position_valid; test_validate_position_invalid ] );
    ]
