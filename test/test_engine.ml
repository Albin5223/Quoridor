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
  |> add_all_players_to_board;
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
  let first_pick_strategy _ = Moving (2, board_size / 2) in

  Alcotest.test_case "Walls cannot be removed" `Quick (fun () ->
      players first_pick_strategy wall_placer |> init_game;
      play ();
      play ();
      Format.printf "Walls : %d\n" (get_all_walls () |> List.length);
      Alcotest.(check int) "Number of walls" 2 (get_all_walls () |> List.length))

let first_pick_strat pos = Moving (list_of_moves pos |> List.hd)

let test_validity_of_first_pick_strat_with_2_players =
  let open QCheck in
  Test.make ~count:100 ~name:"test_validity_of_first_pick_strat" (int_range 2 2)
    (fun n ->
      let _ = create_list_of_player n 10 first_pick_strat |> run_game in
      true)

let test_validity_of_first_pick_strat_with_4_players =
  let open QCheck in
  Test.make ~count:100 ~name:"test_validity_of_first_pick_strat" (int_range 4 4)
    (fun n ->
      let _ = create_list_of_player n 5 first_pick_strat |> run_game in
      true)

let test_validity_of_random_strategy_with_2_players =
  let open QCheck in
  Test.make ~count:100 ~name:"Radom strategy is valid"
    (pair (int_range 2 2) int)
    (fun (n, seed) ->
      Random.init seed;

      let _ = create_list_of_player n 10 Strategy.det_move |> run_game in
      true)

let test_validity_of_random_strategy_with_4_players =
  let open QCheck in
  Test.make ~count:100 ~name:"Radom strategy is valid"
    (pair (int_range 4 4) int)
    (fun (n, seed) ->
      Random.init seed;

      let _ = create_list_of_player n 5 Strategy.det_move |> run_game in
      true)

let test_create_player =
  Alcotest.test_case "create_player" `Quick (fun () ->
      let _ = create_player (-1, 0) 0 Red (fun _ -> Moving (0, 0)) in
      let _ = create_player (800, 0) 0 Red (fun _ -> Moving (0, 0)) in
      let _ = create_player (0, 0) 0 Red (fun _ -> Moving (-1, 0)) in
      let _ = create_player (0, 0) 0 Red (fun _ -> Moving (800, 0)) in
      let _ =
        create_player (0, 0) 0 Red (fun _ -> Placing_wall ((0, 0), (0, 0)))
      in
      ())

let test_add_players =
  Alcotest.test_case "add_players" `Quick (fun () ->
      try
        add_players
          [
            create_player (-1, 0) 0 Red (fun _ -> Moving (0, 0));
            create_player (800, 0) 0 Red (fun _ -> Moving (0, 0));
            create_player (0, 0) 0 Red (fun _ -> Moving (-1, 0));
            create_player (0, 0) 0 Red (fun _ -> Moving (800, 0));
            create_player (0, 0) 0 Red (fun _ -> Placing_wall ((0, 0), (0, 0)));
          ]
      with InvalidPlayerWallsLeft _ | InvalidNumberPlayer _ -> ())

let can_play_two_games =
  let open QCheck in
  Test.make ~count:10 ~name:"Can play two games"
    (pair (int_range 2 2) int)
    (fun (n, seed) ->
      Random.init seed;
      let _ = create_list_of_player n 10 Strategy.det_move |> run_game in
      let _ = create_list_of_player n 10 Strategy.det_move |> run_game in
      true)

let test_3_player_game =
  Alcotest.test_case "3 player game is not allowed" `Quick (fun () ->
      try
        create_list_of_player 3 5 first_pick_strat |> add_players;
        start_game ();
        failwith "3 player game should not be allowed"
      with InvalidNumberPlayer _ -> ())

let test_only_2_and_4_players_are_allowed =
  let open QCheck in
  Test.make ~count:100 ~name:"Only 2 and 4 players are allowed" (int_range 1 10)
    (fun n ->
      try
        if n = 2 then create_list_of_player n 10 first_pick_strat |> add_players
        else create_list_of_player n 5 first_pick_strat |> add_players;
        start_game ();
        if n = 2 || n = 4 then true else false
      with InvalidNumberPlayer _ -> if n = 2 || n = 4 then false else true)

let accepcts_only_n_walls ~nb_walls ~nb_players =
  QCheck.Test.make ~count:1000
    ~name:
      (Printf.sprintf "Accepts only %d walls | %d players" nb_walls nb_players)
    QCheck.(int_range 0 100)
    (fun walls ->
      let players =
        create_list_of_player nb_players nb_walls (fun _ -> Moving (0, 0))
        |> List.map (fun player -> { player with walls_left = walls })
      in
      try
        add_players players;
        if walls = nb_walls then true else false
      with InvalidPlayerWallsLeft _ -> true)

let number_of_walls_is_correct =
  [
    (* Just a dramatic example, QCheck is better for this (see other test cases) *)
    Alcotest.test_case "Can't have 100" `Quick (fun () ->
        let players =
          create_list_of_player 2 10 (fun _ -> Moving (0, 0))
          |> List.map (fun player -> { player with walls_left = 100 })
        in

        try
          add_players players;
          failwith "Players can't have 100 walls"
        with InvalidPlayerWallsLeft _ -> ());
    Alcotest.test_case "Accepts 10 walls | 2 players" `Quick (fun () ->
        let players =
          create_list_of_player 2 10 (fun _ -> Moving (0, 0))
          |> List.map (fun player -> { player with walls_left = 10 })
        in
        add_players players);
    Alcotest.test_case "Accepts 5 walls | 4 players" `Quick (fun () ->
        let players =
          create_list_of_player 4 5 (fun _ -> Moving (0, 0))
          |> List.map (fun player -> { player with walls_left = 5 })
        in
        add_players players);
    QCheck_alcotest.to_alcotest
      (accepcts_only_n_walls ~nb_walls:10 ~nb_players:2);
    QCheck_alcotest.to_alcotest
      (accepcts_only_n_walls ~nb_walls:5 ~nb_players:4);
  ]

(* This test is no longer usable because the do-move function has been hidden

      let test_player_cannot_win_on_first_turn =
        (* Moves a player starting from the top to the bottom of the board. It assumes that that player
           is the second one and that the first one is on the left side of the board. There are only
           two players in the game. *)
        let move_to_the_end_strat _ =
          List.init (board_size - 1) (fun i ->
              if i mod 4 = 0 then Moving (2, board_size / 2)
              else if i mod 2 = 0 then Moving (0, board_size / 2)
              else Moving (board_size / 2, i + 1))
          |> List.iter do_move;
          Moving (2, board_size / 2)
        in
        Alcotest.test_case "Player cannot win on first turn" `Quick (fun () ->
                [
                  (Red, (0, board_size / 2), move_to_the_end_strat);
                  (Blue, (board_size / 2, 0), first_pick_strat);
                ]
                |> init_game;
                try
                  Quoridor.Board.play ();
                  let _ = winning_player () in
                  failwith "Player can win on first turn"
                with NoWinningPlayer _ | InvalidMove _ -> ())*)

let () =
  let open Alcotest in
  run "Engine"
    [
      ("Walls cannot be removed", [ test_walls_cannot_be_removed ]);
      ( "Player number",
        [
          test_3_player_game;
          QCheck_alcotest.to_alcotest test_only_2_and_4_players_are_allowed;
        ] );
      ( "First pick strategy is valid with 2 players",
        [
          QCheck_alcotest.to_alcotest
            test_validity_of_first_pick_strat_with_2_players;
        ] );
      ( "First pick strategy is valid with 4 players",
        [
          QCheck_alcotest.to_alcotest
            test_validity_of_first_pick_strat_with_4_players;
        ] );
      ( "Random Strategy is valid with 4 players",
        [
          QCheck_alcotest.to_alcotest
            test_validity_of_random_strategy_with_4_players;
        ] );
      ( "Random Strategy is valid with 2 players",
        [
          QCheck_alcotest.to_alcotest
            test_validity_of_random_strategy_with_2_players;
        ] );
      ("create_player", [ test_create_player ]);
      ("add_player", [ test_add_players ] @ number_of_walls_is_correct);
      ( "Game integrity",
        [
          QCheck_alcotest.to_alcotest can_play_two_games;
          (*test_player_cannot_win_on_first_turn;*)
        ] );
    ]
