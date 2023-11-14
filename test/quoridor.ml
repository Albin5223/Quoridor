open Quoridor

(* AUXILIARY FUNCTIONS *)

let returns_exception f x =
  try 
    ignore (f x); false
  with
    | _ -> true

let returns_exception_unit f = 
  try
    ignore f; false
  with
    | _ -> true


let qcheck_test name objects f =
  QCheck.Test.make ~count:1000
  ~name: name
  objects f

let qch_to_alc tests_list =
  List.map QCheck_alcotest.to_alcotest tests_list
    
let qch_to_alc_uniq test =
  qch_to_alc [test]

(* TESTS *)

let test_position =
  let random_correct_pos =
    let x = QCheck.(--) 0 16 in
    let y = QCheck.(--) 0 16 in
  QCheck.pair x y
  in 
  qcheck_test "For all positions (x,y) such that x,y in [|0,16|]², (x,y) is a valid position " random_correct_pos (fun pair -> Board.is_valid_position pair)

let test_players = 
  let zero_or_one = QCheck.(--) 5 50 in
  qcheck_test "For all n, init_game n when n in [|5,50|], returns an exception" 
  zero_or_one (fun n -> returns_exception Engine.init_game n)

let test_players_2 () =
  Alcotest.(check bool) "same bool" true (returns_exception Engine.init_game 0)

let test_players_3 () =
  Alcotest.(check bool) "same bool" true (returns_exception Engine.init_game 1)



let test_finite_game = 
  qcheck_test "Game always ends" (QCheck.int)
  (fun _ -> not (returns_exception_unit Engine.run_game))
   

let () =
  let open Alcotest in
    run "Quoridor tests" [
      "correct_positions" , qch_to_alc_uniq test_position;
      "incorrect_nb_of_players" , qch_to_alc_uniq test_players @ 
        [test_case "0 player" `Quick test_players_2; test_case "1 player" `Quick test_players_3];
      "correct_game", qch_to_alc_uniq test_finite_game 
    ];
