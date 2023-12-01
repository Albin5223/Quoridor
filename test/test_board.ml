open Quoridor
open Quoridor.Types
open Utils

let valid_positions =
  let open Quoridor.Board in
  [
    (0, board_size / 2);
    (board_size - 1, board_size / 2);
    (board_size / 2, 0);
    (board_size / 2, board_size - 1);
  ]

let test_add_player_valid =
  Alcotest.test_case "add_player_valid" `Quick (fun () ->
      let player =
        Engine.create_player (0, Board.board_size / 2) 10 Red Strategy.det_move
      in
      Board.add_all_players_to_board [ player ];
      Alcotest.(check bool)
        "Player added correctly" true
        (Board.is_player (0, Board.board_size / 2)))

let add_player_on_invalid_initial_position =
  let open QCheck in
  Test.make ~name:"add_player_on_invalid_initial_positions" ~count:1000
    (pair (int_range 0 16) (int_range 0 16))
    (fun (x, y) ->
      assume (not (List.mem (x, y) valid_positions));
      let player = Engine.create_player (x, y) 10 Red Strategy.det_move in
      try
        Board.add_all_players_to_board [ player ];
        false
      with InvalidPlayerPosition _ -> true)
  |> QCheck_alcotest.to_alcotest

let test_add_players_invalid_color =
  let open QCheck in
  Test.make ~name:"Multiple players with same color" ~count:1000
    (pair arbitrary_color (int_range 1 2))
    (fun (color, nb_players) ->
      let nb_players = 2 * nb_players in
      let players =
        List.init nb_players (fun index ->
            Engine.create_player
              (List.nth valid_positions index)
              (20 / nb_players) color Strategy.det_move)
      in
      try
        Board.add_all_players_to_board players;
        false
      with InvalidPlayerColor _ -> true)
  |> QCheck_alcotest.to_alcotest

let test_add_player_invalid_nb_of_walls =
  let open QCheck in
  Test.make ~name:"Invalid number of walls" ~count:1000 (list small_int)
    (fun walls_left_list ->
      let len = List.length walls_left_list in
      assume (len = 2 || len = 4);
      assume (List.for_all (fun p -> p <> 20 / len) walls_left_list);
      let bi_colors = [ Red; Blue ] and colors = [ Red; Blue; Green; Yellow ] in
      let players =
        mapi2
          (fun index walls_left color ->
            Engine.create_player
              (List.nth valid_positions index)
              walls_left color Strategy.det_move)
          walls_left_list
          (if len = 2 then bi_colors else colors)
      in
      Board.add_all_players_to_board players;
      try
        Board.start_game ();
        false
      with InvalidPlayerWallsLeft _ -> true)
  |> QCheck_alcotest.to_alcotest

let test_winning_player_none =
  Alcotest.test_case "winning_player_none" `Quick (fun () ->
      let p1 =
        Engine.create_player (0, Board.board_size / 2) 10 Red Strategy.det_move
      in
      let p2 = Engine.create_player (8, 0) 10 Yellow Strategy.det_move in
      Board.add_all_players_to_board [ p1; p2 ];
      try
        let _ = Board.winning_player () in
        Alcotest.fail "No exception raised for no winning player"
      with NoWinningPlayer _ -> ())

let test_starting_game =
  Alcotest.test_case "impossible_situation_to_start_game" `Quick (fun () ->
      try
        let player1 =
          Engine.create_player
            (0, Board.board_size / 2)
            10 Types.Red
            (fun _ -> Moving (0, 0))
        in 
        Board.add_all_players_to_board [ player1 ];
        Board.start_game ();
        Alcotest.fail "No exception raised for starting incomplete game"
      with InvalidNumberPlayer _ | InvalidGameState _ -> ())

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
  QCheck.Test.make ~name:"validate_position_prop" ~count:1000
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

let test_is_wall_position_prop =
  QCheck.Test.make ~name:"is_wall_position_validity" ~count:1000
    (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume
        (x >= 0 && y >= 0 && x < Board.board_size && y < Board.board_size);
      Board.is_wall_position (x, y)
      = ((x mod 2 = 1 && y mod 2 = 0) || y mod 2 = 1))

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

let test_is_player_position_prop =
  QCheck.Test.make ~name:"is_player_position_validity" ~count:1000
    (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume
        (x >= 0 && y >= 0 && x < Board.board_size && y < Board.board_size);
      Board.is_player_position (x, y) = (x mod 2 = 0 && y mod 2 = 0))

let test_are_not_player_positions =
  QCheck.Test.make ~name:"invalid player positions" ~count:1000
    (QCheck.pair QCheck.small_int QCheck.small_int) (fun (x, y) ->
      QCheck.assume
        ((x mod 2 <> 0 || y mod 2 <> 0)
        && x >= 0 && y >= 0 && x <= 16 && y <= 16);
      not (Board.is_player_position (x, y)))

(* This test is no longer usable because the do-move function has been hidden
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
         Board.do_move (Placing_wall ((1, 0), (1, 1)));
         Alcotest.(check bool)
           "Wall is present" true
           (Board.is_wall (1, 0) && Board.is_wall (1, 1)))*)

let test_is_player =
  Alcotest.test_case "is_player" `Quick (fun () ->
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

(* This test is no longer usable because the do-move function has been hidden
   let test_is_wall_between =
     Alcotest.test_case "is_wall_between" `Quick (fun () ->
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

         let wall_pos1 = (1, 0) in
         let wall_pos2 = (1, 1) in
         Board.do_move (Placing_wall (wall_pos1, wall_pos2));

         let pos1 = (0, 0) in
         let pos2 = (2, 0) in

         Alcotest.(check bool)
           "Wall is between pos1 and pos2" true
           (Board.is_wall_between pos1 pos2))*)

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ( "validate_wall_placement",
        [ test_is_wall_position; test_is_player_position ] );
      ("is_player", [ test_is_player ]);
      ( "add_player_to_board",
        [
          test_add_player_valid;
          add_player_on_invalid_initial_position;
          test_add_players_invalid_color;
          test_add_player_invalid_nb_of_walls;
        ] );
      ("start_game", [ test_starting_game ]);
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
      ( "is_wall_position_prop",
        List.map QCheck_alcotest.to_alcotest [ test_is_wall_position_prop ] );
      ( "is_player_position_prop",
        List.map QCheck_alcotest.to_alcotest [ test_is_player_position_prop ] );
      ( "equivalence_wallPos_and_playPos",
        List.map QCheck_alcotest.to_alcotest [ test_equiv_wallpos_playpos ] );
      ( "adjacent_functions",
        [ test_invalid_adj_players_position; test_invalid_adj_walls_position ]
      );
      ("list_of_moves", [ test_list_of_moves_invalid_pos ]);
    ]
