open Quoridor.Board
open Quoridor.Types
open Quoridor.Engine
open Utils

let first_pick_strat pos = Moving (list_of_moves pos |> List.hd)

let test_validity_of_first_pick_strat =
  let open QCheck in
  Test.make ~count:100 ~name:"test_validity_of_first_pick_strat" (int_range 2 4)
    (fun n ->
      reset_board ();
      let _ = create_list_of_player n first_pick_strat |> run_game in
      true)

let test_validity_of_random_strategy =
  let open QCheck in
  Test.make ~count:100 ~name:"Radom strategy is valid"
    (pair (int_range 2 4) int)
    (fun (n, seed) ->
      Random.init seed;
      reset_board ();
      let _ = create_list_of_player n Strategy.det_move |> run_game in
      true)

let () =
  let open Alcotest in
  run "Engine"
    [
      ( "First pick strategy is valid",
        [ QCheck_alcotest.to_alcotest test_validity_of_first_pick_strat ] );
      ( "Random Strategy is valid",
        [ QCheck_alcotest.to_alcotest test_validity_of_random_strategy ] );
    ]
