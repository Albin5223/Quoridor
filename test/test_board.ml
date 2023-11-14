open Quoridor.Types

let test_add_player_to_board_valid () =
  Alcotest.test_case "add_player_to_board - valid" `Quick (fun () ->
    let open Quoridor.Board in
    add_player_to_board Red (0, 8);
  )

  (*
let test_add_player_to_board_invalid_position () =
  Alcotest.test_case "add_player_to_board - invalid position" `Quick (fun () ->
    let open Quoridor.Board in
    Alcotest.check_raises "InvalidPosition exception" 
      (InvalidPosition ((-1, -1), "Position is outside the board boundaries"))
      (fun () -> add_player_to_board Red (-1, -1))
  )

let test_pos_current_player () =
  Alcotest.test_case "pos_current_player" `Quick (fun () ->
    let open Quoridor.Board in
    add_player_to_board Red (0, 8);
    Alcotest.(check (pair int int)) "Current player position" (0, 8) (pos_current_player ())
  )

let test_walls_left_current_player () =
  Alcotest.test_case "walls_left_current_player" `Quick (fun () ->
    let open Quoridor.Board in
    add_player_to_board Red (0, 8);
    Alcotest.(check int) "Walls left for current player" 10 (walls_left_current_player ())
  )

  *)

(** {2 Test execution} *)
let () =
  let open Alcotest in
  run "Board Module Tests"
    [
      "add_player_to_board", [
        test_add_player_to_board_valid ();
        (*test_add_player_to_board_invalid_position ();*)
      ];
      "pos_current_player", [
        (*test_pos_current_player ();*)
      ];
      "walls_left_current_player", [
        (*test_walls_left_current_player ();*)
      ];
    ]
