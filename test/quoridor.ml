open Quoridor
let reference_game = Engine.init_game 4

let test_players () = Alcotest.(check int) "same int" 4 (List.length reference_game.players)

let test_invalid_position () = Alcotest.(check bool) "same_bool" false (Board.is_valid_position (-1,-1))
let test_valid_position () = Alcotest.(check bool) "same_bool" true (Board.is_valid_position (6,6))
let test_board_size () = 
  let board = Engine.init_board in
  Alcotest.(check int) "same_int" 17 (Array.length board)


let returns_exception f x =
  try 
    let _ = f x in true
  with
    | _ -> false


let random_nb_players = 
  QCheck.(--) 2 4


let test = 
  QCheck.Test.make ~count:1000
  ~name: "ppp"
  random_nb_players
  (fun l -> l <= 4 && l >= 2)

let qcheck_test name objects f =
  QCheck.Test.make ~count:1000
  ~name: name
  objects f

let test_position =
  let random_correct_pos =
    let x = QCheck.(--) 0 16 in
    let y = QCheck.(--) 0 16 in
  QCheck.pair x y
  in 
  qcheck_test "For all positions (x,y) such that x,y in [|0,16|]², (x,y) is a valid position " random_correct_pos (fun pair -> Board.is_valid_position pair)

let qch_to_alc tests_list =
  List.map QCheck_alcotest.to_alcotest tests_list

let qch_to_alc_uniq test =
  qch_to_alc [test]
    
  
  (*
  let passing =
    QCheck.Test.make ~count:1000
      ~name:"list_rev_is_involutive"
      QCheck.(list small_int)
      (fun l -> List.rev (List.rev l) = l);;

let suite =
  List.map QCheck_alcotest.to_alcotest [passing]
  *)

let () =
  let open Alcotest in
  run "Quoridor tests" [
    "correct-position", [
      test_case "Out of bounds coordinates" `Quick test_invalid_position;
      test_case "Correct coordinates" `Quick test_valid_position;
    ];
    "board-length", [
      test_case "Correct length of 17" `Quick test_board_size;
    ];
    "players-number", [
      test_case "4 players" `Quick test_players;
    ];
    "test_to_delete" , qch_to_alc_uniq test;
    "correct_positions" , qch_to_alc_uniq test_position;
  ];
