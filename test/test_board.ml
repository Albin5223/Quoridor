open Quoridor
open Quoridor.Types

module Strategy = struct
  open Quoridor.Types
  open Quoridor.Board

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

let test_add_player_valid =
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

let test_add_player_invalid_color =
  Alcotest.test_case "invalid_color" `Quick (fun () ->
      Board.reset_board ();
      let p1 = Engine.create_player (8, 16) 10 Red Strategy.det_move in
      let p2 = Engine.create_player (8, 0) 10 Red Strategy.det_move in
      Board.add_player_to_board p1;
      try
        Board.add_player_to_board p2;
        Board.start_game ();
        Alcotest.fail "Unraised exception when two players have the same color"
      with InvalidPlayerColor _ -> ())

let test_add_player_invalid_am_of_walls =
  Alcotest.test_case "invalid number of walls" `Quick (fun () ->
      Board.reset_board ();
      try
        let p1 = Engine.create_player (8, 16) 777 Red Strategy.det_move in
        Board.add_player_to_board p1;
        Board.start_game ()
      with InvalidPlayerWallsLeft _ -> ())

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

let test_starting_game =
  Alcotest.test_case "impossible_situation_to_start_game" `Quick (fun () ->
      Board.reset_board ();
      try
        Board.start_game ();
        Alcotest.fail "No exception raised for starting incomplete game"
      with InvalidNumberPlayer _ -> ())

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
        Board.validate_position (5, 5);
        Alcotest.(check bool) "No exception for valid position" true true
      with _ -> Alcotest.fail "Unexpected exception for valid position")

let test_validate_position_invalid =
  Alcotest.test_case "validate_position_invalid" `Quick (fun () ->
      try
        Board.validate_position (-1, 5);
        Alcotest.fail "Expected exception for invalid position"
      with
      | InvalidPosition _ -> ()
      | _ -> Alcotest.fail "Unexpected exception type for invalid position")

let test_validate_position_prop =
  QCheck.Test.make ~name:"validate_position_prop"
    QCheck.(pair int int)
    (fun (x, y) ->
      try
        Board.validate_position (x, y);
        x >= 0 && x < Board.board_size && y >= 0 && y < Board.board_size
      with
      | InvalidPosition _ ->
          x < 0 || x >= Board.board_size || y < 0 || y >= Board.board_size
      | _ -> false)

let test_is_wall_position =
  Alcotest.test_case "is_wall_position" `Quick (fun () ->
      Alcotest.(check bool)
        "Wall position (1,0)" true
        (Board.is_wall_position (1, 0));
      Alcotest.(check bool)
        "Not wall position (0,0)" false
        (Board.is_wall_position (0, 0)))

let test_are_not_wall_positions =
  QCheck.Test.make ~name:"invalid wall positions" ~count:1000
    (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume
        ((x mod 2 <> 1 || y mod 2 <> 0)
        && y mod 2 <> 1
        && x >= 0 && y >= 0 && x <= 16 && y <= 16);
      not (Board.is_wall_position (x, y)))

let test_is_player_position =
  Alcotest.test_case "is_player_position" `Quick (fun () ->
      Alcotest.(check bool)
        "Player position (0,0)" true
        (Board.is_player_position (0, 0));
      Alcotest.(check bool)
        "Not player position (1,0)" false
        (Board.is_player_position (1, 0)))

let test_are_not_player_positions =
  QCheck.Test.make ~name:"invalid player positions" ~count:1000
    (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume
        ((x mod 2 <> 0 || y mod 2 <> 0)
        && x >= 0 && y >= 0 && x <= 16 && y <= 16);
      not (Board.is_player_position (x, y)))

let test_is_wall =
  Alcotest.test_case "is_wall" `Quick (fun () ->
      Board.reset_board ();
      let player1 =
        Engine.create_player
          (0, Board.board_size / 2)
          10 Types.Red
          (fun _ -> Moving (0, 0))
      in
      let player2 =
        Engine.create_player
          (Board.board_size - 1, Board.board_size / 2)
          10 Types.Green
          (fun _ -> Moving (0, 0))
      in
      Engine.add_players [ player1; player2 ];
      Board.start_game ();
      Board.place_wall (1, 0) (1, 1);
      Alcotest.(check bool)
        "Wall is present" true
        (Board.is_wall (1, 0) && Board.is_wall (1, 1)))

let test_is_player =
  Alcotest.test_case "is_player" `Quick (fun () ->
      Board.reset_board ();
      let player1 =
        Engine.create_player
          (0, Board.board_size / 2)
          10 Types.Red
          (fun _ -> Moving (0, 0))
      in
      let player2 =
        Engine.create_player
          (Board.board_size - 1, Board.board_size / 2)
          10 Types.Green
          (fun _ -> Moving (0, 0))
      in
      Engine.add_players [ player1; player2 ];
      Board.start_game ();
      let player_pos = (0, Board.board_size / 2) in
      Alcotest.(check bool)
        "Player is present" true
        (Board.is_player player_pos))

let test_equiv_wallpos_playpos =
  QCheck.Test.make
    ~name:"is_wall_position <=> is_not_player_position and vice versa"
    ~count:1000 (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume (x >= 0 && y >= 0 && x <= 16 && y <= 16);
      let p = Board.is_wall_position (x, y) in
      let q = Board.is_player_position (x, y) in
      let left_impl = QCheck.( ==> ) p (not q) in
      let right_impl = QCheck.( ==> ) (not p) q in
      left_impl && right_impl)

let test_invalid_pos_function string f =
  Alcotest.test_case string `Quick (fun () ->
      try f (-1, -1) |> ignore with InvalidPosition _ -> ())

let test_invalid_adj_players_position =
  test_invalid_pos_function
    "adjacent_players raises an exception for invalid position"
    Board.adjacent_players

let test_invalid_adj_walls_position =
  test_invalid_pos_function
    "adjacent_walls raises an exception for invalid position"
    Board.adjacent_walls

let test_list_of_moves_invalid_pos =
  test_invalid_pos_function
    "list_of_moves raises an exception for invalid position" Board.list_of_moves

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ( "validate_wall_placement",
        [ test_is_wall_position; test_is_player_position ] );
      ("is_wall", [ test_is_wall ]);
      ("is_player", [ test_is_player ]);
      ( "add_player_to_board",
        [
          test_add_player_valid;
          test_add_player_invalid_position;
          test_add_player_invalid_color;
          test_add_player_invalid_am_of_walls;
        ] );
      ("start_game", [ test_starting_game ]);
      ("move_player", [ test_move_player_valid; test_move_player_invalid ]);
      ("place_wall", [ test_place_wall_valid; test_place_wall_invalid ]);
      ("winning_player", [ test_winning_player_none ]);
      ( "validate_position",
        [ test_validate_position_valid; test_validate_position_invalid ] );
      ( "validate_position_prop",
        [ QCheck_alcotest.to_alcotest test_validate_position_prop ] );
      ( "wall_positions",
        List.map QCheck_alcotest.to_alcotest [ test_are_not_wall_positions ] );
      ( "player_positions",
        List.map QCheck_alcotest.to_alcotest [ test_are_not_player_positions ]
      );
      ( "equivalence_wallPos_and_playPos",
        List.map QCheck_alcotest.to_alcotest [ test_equiv_wallpos_playpos ] );
      ( "adjacent_functions",
        [ test_invalid_adj_players_position; test_invalid_adj_walls_position ]
      );
      ("list_of_moves", [ test_list_of_moves_invalid_pos ]);
    ]
