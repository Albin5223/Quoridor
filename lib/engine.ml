open Types
open Board

let create_player pos walls_left color strat =
  { position = pos; walls_left; color; strategy = strat }

let add_players player_lst =
  List.iter (fun pl -> Board.add_player_to_board pl) player_lst

let play () =
  let strat = (current_player ()).strategy in
  let pos = (current_player ()).position in
  let move = strat pos in
  match move with
  | Wall (pos1, pos2) -> Board.place_wall pos1 pos2
  | Moving pos -> Board.move_player pos

let run_game player_lst =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let winner = Board.winning_player () in
      Format.printf "Player %s win\n" (color_to_string winner)
    with NoWinningPlayer _ ->
      play ();
      aux ()
  in
  add_players player_lst;
  Board.start_game ();
  aux ()
