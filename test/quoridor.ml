open Quoridor
(* let reference_game = Engine.init_game 4 *)

let returns_exception f x =
  try 
    let _ = f x in true
  with
    | _ -> false


(* let random_nb_players = 
  QCheck.(--) 2 4 *)

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

let test_players = 
  let ppp = QCheck.(--) 0 1 in
  qcheck_test "For all n, init_game n when n in [|0,1|], returns an exception" 
  ppp (fun n -> returns_exception Engine.init_game n)
    
  
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
    "correct_positions" , qch_to_alc_uniq test_position;
    "incorrect_nb_of_players" , qch_to_alc_uniq test_players;
  ];
