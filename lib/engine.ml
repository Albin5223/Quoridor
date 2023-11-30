open Types
open Board

let create_player pos walls_left color strat =
  {
    start_position = pos;
    current_position = pos;
    walls_left;
    color;
    strategy = strat;
  }

let add_players player_lst =
  let num_players = List.length player_lst in
  if num_players <> 2 && num_players <> 4 then
    raise
      (InvalidNumberPlayer
         (num_players, "Number of players must be 2 or 4 to start the game"))
  else if
    not (List.for_all (fun p -> p.walls_left = 20 / num_players) player_lst)
  then
    raise
      (InvalidPlayerWallsLeft
         "The number of walls for each player is not allowed")
  else add_all_players_to_board player_lst

let run_game player_lst =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let winner = Board.winning_player () in
      Format.printf "Player %s won\n" (color_to_string winner)
    with NoWinningPlayer _ ->
      Board.play ();
      aux ()
  in
  add_players player_lst;
  Board.start_game ();
  aux ()
