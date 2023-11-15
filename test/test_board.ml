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
    let p1 = Engine.create_player (8,16) 10 Red Strategy.det_move in 
    let p2 = Engine.create_player (8,0) 10 Red Strategy.det_move in 
    Board.add_player_to_board p1;
    try 
      Board.add_player_to_board p2; 
      Board.start_game (); 
      Alcotest.fail "Unraised exception when two players have the same color"
    with
      InvalidPlayerColor _ -> ())

let test_add_player_invalid_am_of_walls = 
  Alcotest.test_case "invalid number of walls" `Quick (fun () -> 
    Board.reset_board ();
    try 
      let p1 = Engine.create_player (8,16) 777 Red Strategy.det_move in 
      Board.add_player_to_board p1; Board.start_game ();
    with 
      InvalidPlayerWallsLeft _ -> ())
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
    with
      InvalidNumberPlayer _  -> ())


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
      with InvalidPosition _ -> ())

let test_are_not_wall_positions =
  QCheck.Test.make ~name: "invalid wall positions" ~count:1000
  (QCheck.pair QCheck.small_int QCheck.small_int)
  (fun (x,y) -> 
    QCheck.assume((x mod 2 <> 1 || y mod 2 <> 0) && y mod 2 <> 1 && x>=0 && y>=0 && x<=16 && y<=16);
    not (Board.is_wall_position (x,y)))

let test_are_not_player_positions =
  QCheck.Test.make ~name: "invalid player positions" ~count:1000
  (QCheck.pair QCheck.small_int QCheck.small_int)
  (fun (x,y) -> 
    QCheck.assume((x mod 2 <> 0 || y mod 2 <> 0) && x>=0 && y>=0 && x<=16 && y<=16);
    not (Board.is_player_position (x,y)))

let test_equiv_wallpos_playpos =
  QCheck.Test.make ~name: "is_wall_position <=> is_not_player_position and vice versa" ~count:1000
  (QCheck.pair QCheck.small_int QCheck.small_int)
  (fun (x,y) -> 
    QCheck.assume (x>=0 && y>=0 && x<=16 && y<=16);
    let p = Board.is_wall_position (x,y) in let q = Board.is_player_position (x,y)
    in let left_impl = QCheck.(==>) p (not q)
    in let right_impl = QCheck.(==>) (not p) q in 
    left_impl && right_impl
    )


let test_invalid_pos_function string f =
  Alcotest.test_case string
  `Quick (fun () -> 
    try 
      f (-1,-1) |> ignore
    with
      InvalidPosition _ -> ())
let test_invalid_adj_players_position =
  test_invalid_pos_function "adjacent_players raises an exception for invalid position"
  (Board.adjacent_players)

let test_invalid_adj_walls_position =
  test_invalid_pos_function "adjacent_walls raises an exception for invalid position"
  (Board.adjacent_walls)
let test_list_of_moves_invalid_pos =
  test_invalid_pos_function "list_of_moves raises an exception for invalid position"
  (Board.list_of_moves)

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ( "add_player_to_board",
        [ test_add_player_valid; test_add_player_invalid_position; 
        test_add_player_invalid_color; test_add_player_invalid_am_of_walls] );
      ("start_game", [test_starting_game]);
      ("move_player", [ test_move_player_valid; test_move_player_invalid ]);
      ("place_wall", [ test_place_wall_valid; test_place_wall_invalid ]);
      ("winning_player", [ test_winning_player_none ]);
      ( "validate_position",
        [ test_validate_position_valid; test_validate_position_invalid ] );
      "wall_positions", List.map (QCheck_alcotest.to_alcotest) [test_are_not_wall_positions];
      "player_positions", List.map (QCheck_alcotest.to_alcotest) [test_are_not_player_positions];
      "equivalence_wallPos_and_playPos", List.map (QCheck_alcotest.to_alcotest) [test_equiv_wallpos_playpos];
      ("adjacent_functions", [test_invalid_adj_players_position; test_invalid_adj_walls_position]);
      ("list_of_moves", [test_list_of_moves_invalid_pos])
    ]
