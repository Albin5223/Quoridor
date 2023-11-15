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

let test_position2 =
  qcheck_test "For all positions, out of bounds positions are invalid ones" 
  (QCheck.pair QCheck.small_int QCheck.small_int)
  (fun (x,y) -> QCheck.assume (x <= -1 || x>=17 || y <= -1 || y >= 17); not (Board.is_valid_position (x,y)))

let test_players = 
  qcheck_test "For all n, init_game n when n not in [|2,4|], returns an exception" 
  QCheck.small_int (fun n -> 
    QCheck.assume (n <= 1 || n >= 5);
    returns_exception Engine.init_game n)

let test_players_2 () =
  Alcotest.(check bool) "same bool" true (returns_exception Engine.init_game 0)

let test_players_3 () =
  Alcotest.(check bool) "same bool" true (returns_exception Engine.init_game 1)

let test_finite_game = 
  qcheck_test "Game always ends" (QCheck.int)
  (fun _ -> not (returns_exception_unit Engine.run_game))

let reference_game = Engine.init_game 4

let player n  = List.nth reference_game.players n 

let p1 = player 0 

let p2 = player 1

let p3 = player 2

let p4 = player 3

let correct_length () = Alcotest.(check int) "same int" 17 (Board.board_size)

let players_length () = Alcotest.(check int) "same int" 4 (List.length reference_game.players)

let test_player_color = 
  qcheck_test "For all games, color atribution is not random" (QCheck.small_int)
  (fun _ -> p1.color = Types.Red && p2.color = Types.Blue && p3.color = Types.Green && p4.color = Types.Yellow) 

let test_walls_left () =
  Alcotest.(check bool) "same int" true 
  (p1.walls_left = 10 && p2.walls_left = 10 && p3.walls_left = 10 && p4.walls_left = 10) (* for_all doesnt work *)

let list_of_moves_are_valid_pos () =  
  let test_pos (player: Types.player) = 
    List.for_all (fun position -> Board.is_valid_position position) (Engine.list_of_moves player.position)
  in 
    Alcotest.(check bool) "same bool" true
    (test_pos p1 && test_pos p2 && test_pos p3 && test_pos p4)



let () =
  let open Alcotest in
    run "Quoridor tests" [
      "correct_positions" , qch_to_alc_uniq test_position;
      "incorrect positions", qch_to_alc_uniq test_position2;
      "incorrect_nb_of_players" , qch_to_alc_uniq test_players @ 
        [test_case "0 player" `Quick test_players_2; test_case "1 player" `Quick test_players_3];
      "finite_game", qch_to_alc_uniq test_finite_game;
      "correct_board_length", [test_case "Board length is 17" `Quick correct_length];
      "correct_player_length", [test_case "There are 4 players" `Quick players_length];
      "correct_color", qch_to_alc_uniq test_player_color;
      "walls_left_amount", [test_case "All players have 10 starting walls" `Quick test_walls_left];
      "valid_list_of_moves", [test_case "List of moves are valid positions" `Quick list_of_moves_are_valid_pos]
    ];
