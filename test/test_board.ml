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
        validate_wall_placement (current_player ()) (x1, y1) (x1 + xv, y1 + yv);
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
    if r == 0 && (current_player ()).walls_left > 0 then pos_wall_random ()
    else random_move pos
end

let test_start_game_with_valid_number_of_players () =
  Alcotest.test_case "start_game with 2-4 players" `Quick (fun () ->
      let players = [Engine.create_player (0, 8) 10 Red (Strategy.det_move);
                     Engine.create_player (16, 8) 10 Blue (Strategy.det_move)] in
      Board.reset_board ();
      Engine.init_game players;
      try
        Board.start_game ();
        Board.stop_game None;
        ()
      with
      | _ -> Alcotest.fail "Unexpected exception type"
      
  )

let test_start_game_with_invalid_number_of_players () =
  Alcotest.test_case "start_game with invalid number of players" `Quick (fun () ->
      let players = [Engine.create_player (0, 8) 10 Red (Strategy.det_move)] in
      Board.reset_board ();
      Engine.init_game players;
      try
        Board.start_game ();
        Alcotest.fail "Expected InvalidNumberPlayer exception"
      with
      | InvalidNumberPlayer (num, _) when num = 1 -> ()
      | _ -> Alcotest.fail "Unexpected exception type"
  )

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ("start_game", [ test_start_game_with_valid_number_of_players ();
                             test_start_game_with_invalid_number_of_players () ]);
    ]

(* let test_add_player_valid =
  Alcotest.test_case "add_player_valid" `Quick (fun () ->
      Board.reset_board ();
      let player =
        Engine.create_player (0, Board.board_size / 2) 10 Red Strategy.det_move
      in
      Board.add_player_to_board player;
      Alcotest.(check bool)
        "Player added correctly" true
        (Board.is_player (0, Board.board_size / 2)))

let test_add_player_invalid_position =
  Alcotest.test_case "add_player_invalid_position" `Quick (fun () ->
      Board.reset_board ();
      try
        let player = Engine.create_player (1, 1) 10 Blue Strategy.det_move in
        Board.add_player_to_board player;
        Alcotest.fail "No exception raised for invalid position"
      with InvalidPlayerPosition _ -> ())

let test_move_player_valid =
  Alcotest.test_case "move_player_valid" `Quick (fun () ->
      Board.reset_board ();
      let p1 = Engine.create_player (8, 16) 10 Red Strategy.det_move in
      Board.add_player_to_board p1;
      let p2 = Engine.create_player (8, 0) 10 Blue Strategy.det_move in
      Board.add_player_to_board p2;
      Board.start_game ();
      Board.move_player (8, 14);
      Alcotest.(check bool)
        "Player moved correctly" true
        (Board.is_player (8, 14)))

let test_move_player_invalid =
  Alcotest.test_case "move_player_invalid" `Quick (fun () ->
      Board.reset_board ();
      let p1 = Engine.create_player (8, 16) 10 Red Strategy.det_move in
      Board.add_player_to_board p1;
      let p2 = Engine.create_player (8, 0) 10 Blue Strategy.det_move in
      Board.add_player_to_board p2;
      Board.start_game ();
      try
        Board.move_player (8, 13);
        Alcotest.fail "No exception raised for invalid move"
      with InvalidMove _ -> ())

let test_place_wall_valid =
  Alcotest.test_case "place_wall_valid" `Quick (fun () ->
      Board.reset_board ();
      let p1 = Engine.create_player (8, 16) 10 Red Strategy.det_move in
      Board.add_player_to_board p1;
      let p2 = Engine.create_player (8, 0) 10 Blue Strategy.det_move in
      Board.add_player_to_board p2;
      Board.start_game ();
      Board.place_wall (1, 0) (1, 1);
      Alcotest.(check bool)
        "Wall placed correctly" true
        (Board.is_wall (1, 0) && Board.is_wall (1, 1)))

let test_place_wall_invalid =
  Alcotest.test_case "place_wall_invalid" `Quick (fun () ->
      Board.reset_board ();
      let p1 = Engine.create_player (8, 16) 10 Red Strategy.det_move in
      Board.add_player_to_board p1;
      let p2 = Engine.create_player (8, 0) 10 Blue Strategy.det_move in
      Board.add_player_to_board p2;
      Board.start_game ();
      try
        Board.place_wall (1, 1) (1, 3);
        Alcotest.fail "No exception raised for invalid wall placement"
      with InvalidWallPosition _ -> ())

let test_winning_player_none =
  Alcotest.test_case "winning_player_none" `Quick (fun () ->
      Board.reset_board ();
      let p1 =
        Engine.create_player (0, Board.board_size / 2) 10 Red Strategy.det_move
      in
      Board.add_player_to_board p1;
      let p2 = Engine.create_player (8, 0) 10 Yellow Strategy.det_move in
      Board.add_player_to_board p2;
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
      with InvalidPosition _ -> ()) *)



(* let () =
  let open Alcotest in
  run "Board Tests"
    [
      (* ( "add_player_to_board",
        [ test_add_player_valid; test_add_player_invalid_position ] );
      ("move_player", [ test_move_player_valid; test_move_player_invalid ]);
      ("place_wall", [ test_place_wall_valid; test_place_wall_invalid ]);
      ("winning_player", [ test_winning_player_none ]);
      ( "validate_position",
        [ test_validate_position_valid; test_validate_position_invalid ] ); *)
      
    ]
*)