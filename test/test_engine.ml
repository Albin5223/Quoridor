(*open Quoridor
  open Quoridor.Types

  module Strategy = struct
    open Quoridor.Types
    open Quoridor.Board

    let random_move pos =
      let lstMv = list_of_moves pos in
      match lstMv with
      | [] -> raise (NoMove "There is no movement possible for this player")
      | _ ->
          let r = Random.int (List.length lstMv) in
          let newPos = List.nth lstMv r in
          Moving newPos

    let pos_wall_random () =
      let rec generate_random_wall_pos () =
        let x1 = Random.int board_size in
        let y1 = Random.int board_size in
        let r = Random.int 4 in
        let xv, yv = List.nth move_vectors r in
        try
          validate_wall_placement
            (current_player ())
            (x1, y1)
            (x1 + xv, y1 + yv);
          ((x1, y1), (x1 + xv, y1 + yv))
        with
        | InvalidWallPosition _ -> generate_random_wall_pos ()
        | InvalidPosition _ -> generate_random_wall_pos ()
        | InvalidWallPlacement _ -> generate_random_wall_pos ()
      in
      let wall_pos1, wall_pos2 = generate_random_wall_pos () in
      Wall (wall_pos1, wall_pos2)

    let det_move pos =
      let r = Random.int 3 in
      if r == 0 && (current_player ()).walls_left > 0 then pos_wall_random ()
      else random_move pos
  end

  let test_init_game =
    Alcotest.test_case "init_game" `Quick (fun () ->
        Board.reset_board ();
        let player_attributes =
          [
            (Red, (0, Board.board_size / 2), Strategy.det_move);
            (Blue, (Board.board_size / 2, 0), Strategy.det_move);
          ]
        in
        Engine.init_game player_attributes;
        let is_red_player = Board.is_player (0, Board.board_size / 2) in
        let is_blue_player = Board.is_player (Board.board_size / 2, 0) in
        Alcotest.(check bool) "Red player added" true is_red_player;
        Alcotest.(check bool) "Blue player added" true is_blue_player)

  let test_play =
    Alcotest.test_case "play" `Quick (fun () ->
        Board.reset_board ();
        let player_attributes =
          [
            (Red, (0, Board.board_size / 2), Strategy.det_move);
            (Blue, (Board.board_size / 2, 0), Strategy.det_move);
          ]
        in
        Engine.init_game player_attributes;
        Board.start_game ();
        Engine.play ();
        Alcotest.(check unit) "Player played a turn" () ())

  let () =
    let open Alcotest in
    run "Engine Tests"
      [ ("init_game", [ test_init_game ]); ("play", [ test_play ]) ]
*)
