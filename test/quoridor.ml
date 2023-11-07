open Quoridor

let test_invalid_position () = Alcotest.(check bool) "same_bool" false (Board.is_valid_position (-1,-1))
let test_valid_position () = Alcotest.(check bool) "same_bool" true (Board.is_valid_position (6,6))
let test_board_size () = 
  let board = Engine.init_board in
  Alcotest.(check int) "same_int" 17 (Array.length board)

let () =
  let open Alcotest in
  run "Board functions" [
    "correct-position", [
      test_case "Out of bounds coordinates" `Quick test_invalid_position;
      test_case "Correct coordinates" `Quick test_valid_position;
    ];
    "board-length", [
      test_case "Correct length of 17" `Quick test_board_size;
    ];
  ]