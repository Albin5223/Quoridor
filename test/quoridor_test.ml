open Quoridor.Types
open Quoridor.Board

(** Test cases for is_valid_position function *)
let test_is_valid_position =
  Alcotest.test_case "is_valid_position" `Quick (fun () ->
      let check_pos pos expected =
        Alcotest.(check bool) (Printf.sprintf "is_valid_position (%d, %d)" (fst pos) (snd pos)) expected (is_valid_position pos)
      in
      check_pos (0, 0) true;
      check_pos (16, 16) true;
      check_pos (17, 17) false;
      check_pos (-1, -1) false)

(* TODO: Add test for get_cell_content *)

let test_is_wall =
  Alcotest.test_case "is_wall" `Quick (fun () ->
      Alcotest.(check bool) "Wall" true (is_wall Wall);
      Alcotest.(check bool) "Not Wall" false (is_wall (Player { position = (0, 0); walls_left = 10; color = Red }))
  )

let test_is_player =
  Alcotest.test_case "is_player" `Quick (fun () ->
      Alcotest.(check bool) "Player" true (is_player (Player { position = (0, 0); walls_left = 10; color = Red }));
      Alcotest.(check bool) "Not Player" false (is_player Wall)
  )

(** Test cases for is_wall_position function *)
let test_is_wall_position =
  Alcotest.test_case "is_wall_position" `Quick (fun () ->
      let check_wall_pos pos expected =
        Alcotest.(check bool) (Printf.sprintf "is_wall_position (%d, %d)" (fst pos) (snd pos)) expected (is_wall_position pos)
      in
      check_wall_pos (1, 1) true;
      check_wall_pos (2, 2) false;
      check_wall_pos (3, 3) true;
      Alcotest.check_raises "invalid position" (InvalidPosition "Position is not valid on the board") (fun () -> ignore (is_wall_position (17, 17)))
  )

(** Test cases for is_player_position function *)
let test_is_player_position =
  Alcotest.test_case "is_player_position" `Quick (fun () ->
      let check_player_pos pos expected =
        Alcotest.(check bool) (Printf.sprintf "is_player_position (%d, %d)" (fst pos) (snd pos)) expected (is_player_position pos)
      in
      check_player_pos (0, 0) true;
      check_player_pos (1, 1) false;
      check_player_pos (2, 2) true;
      Alcotest.check_raises "invalid position" (InvalidPosition "Position is not valid on the board") (fun () -> ignore (is_player_position (17, 17)))
  )

(* TODO : Add tests for list_of_walls & list_of_players *)

let test_is_wall_between =
  Alcotest.test_case "is_wall_between" `Quick (fun () ->
      let board = Array.make_matrix board_size board_size Empty in
      board.(0).(1) <- Wall;
      let check_wall_between pos1 pos2 expected =
        Alcotest.(check bool)
          (Printf.sprintf "is_wall_between (%d, %d) and (%d, %d)" (fst pos1) (snd pos1) (fst pos2) (snd pos2))
          expected
          (is_wall_between pos1 pos2 board)
      in
      check_wall_between (0, 0) (2, 0) true;
      check_wall_between (0, 0) (0, 2) false;
      Alcotest.check_raises "same positions" (InvalidPosition "The two positions are the same") (fun () -> ignore (is_wall_between (0, 0) (0, 0) board));
      Alcotest.check_raises "out of bounds" (OutOfBounds "One of the positions is outside the board boundaries") (fun () -> ignore (is_wall_between (0, 0) (17, 17) board));
      Alcotest.check_raises "odd coordinates" (InvalidPosition "Positions must be even coordinates") (fun () -> ignore (is_wall_between (1, 1) (3, 1) board))
  )

let test_list_of_moves =
  Alcotest.test_case "list_of_moves" `Quick (fun () ->
      let board = Array.make_matrix board_size board_size Empty in
      board.(0).(1) <- Wall;  (* Wall to the right of (0, 0) *)
      board.(1).(0) <- Wall;  (* Wall below (0, 0) *)
      let check_moves pos expected =
        let moves = list_of_moves pos board in
        Alcotest.(check (array (pair int int))) 
          (Printf.sprintf "list_of_moves (%d, %d)" (fst pos) (snd pos)) 
          expected 
          moves
      in
      check_moves (0, 0) [| (0, 2); (2, 0) |];  (* Only upward and leftward moves are possible *)
      check_moves (2, 2) [| (0, 2); (2, 0); (2, 4); (4, 2) |];  (* All directions are possible *)
      Alcotest.check_raises "out of bounds" 
        (OutOfBounds "Position is outside the board boundaries") 
        (fun () -> ignore (list_of_moves (17, 17) board));
      Alcotest.check_raises "wall position" 
        (InvalidWallPosition "Given position is a wall position") 
        (fun () -> ignore (list_of_moves (0, 1) board));
      Alcotest.check_raises "not a player's position" 
        (InvalidPosition "Given position is not a player's position") 
        (fun () -> ignore (list_of_moves (1, 1) board))
  )
  


(** Property-based test for is_valid_position function *)
let test_is_valid_position_property =
  let open QCheck in
  Test.make
    ~count:100
    ~name:"forall pos, is_valid_position pos = (x >= 0 && x < board_size && y >= 0 && y < board_size)"
    (pair int int)
    (fun pos ->
      let x, y = pos in
      is_valid_position pos = (x >= 0 && x < board_size && y >= 0 && y < board_size)
    )

(******************************************************************)

let () =
  let open Alcotest in
  run "Board Tests"
    [
      ("unit", [ test_is_valid_position; test_is_wall; test_is_player; test_is_wall_position; test_is_player_position; test_is_wall_between; test_list_of_moves ]);
      ("qcheck", [ QCheck_alcotest.to_alcotest test_is_valid_position_property ]);
    ]
