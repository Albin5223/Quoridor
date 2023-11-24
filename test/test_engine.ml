open Quoridor.Types
open Quoridor.Board
open Quoridor.Engine
open Utils

let init_game player_info =
  player_info
  |> List.map (fun (color, pos, strat) ->
         {
           color;
           start_position = pos;
           current_position = pos;
           strategy = strat;
           walls_left = 10;
         })
  |> List.iter add_player_to_board;
  start_game ()

let get_all_walls () =
  List.init board_size (fun x -> x)
  |> List.map (fun x -> List.init board_size (fun y -> (x, y)))
  |> List.flatten |> List.filter is_wall

let test_walls_cannot_be_removed =
  let players strat1 strat2 =
    [ (Red, (0, board_size / 2), strat1); (Blue, (board_size / 2, 0), strat2) ]
  in
  let wall_placer _ =
    let wall_pos1 = (1, 0) in
    let wall_pos2 = (1, 1) in
    Placing_wall (wall_pos1, wall_pos2)
  in
  let rec reseter_strat _ =
    reset_board ();
    players wall_placer reseter_strat |> init_game;
    (* This should be the actual test :
       `first_pick_strategy pos`
       but since the current player has been updated, `pos` means nothing, as it is
       the position of the player who just played. So I joust hardcoded a position that
       t should work is walls are immutable but current player changes thanks to `reset_board`*)
    Moving (2, board_size / 2)
  in

  Alcotest.test_case "Walls cannot be removed" `Quick (fun () ->
      reset_board ();
      players reseter_strat wall_placer |> init_game;
      play ();
      play ();
      Alcotest.(check int) "Number of walls" 2 (get_all_walls () |> List.length))

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
  run "Engine" [ ("Walls cannot be removed", [ test_walls_cannot_be_removed ]);
  ( "First pick strategy is valid",
        [ QCheck_alcotest.to_alcotest test_validity_of_first_pick_strat ] );
      ( "Random Strategy is valid",
        [ QCheck_alcotest.to_alcotest test_validity_of_random_strategy ] ); ]
