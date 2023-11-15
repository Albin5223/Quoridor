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
  List.iter (fun pl -> Board.add_player_to_board pl) player_lst

let play () =
  let strat = (current_player ()).strategy in
  let pos = (current_player ()).current_position in
  let move = strat pos in
  Board.do_move move
  

let run_game player_lst =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let winner = Board.winning_player () in
      Format.printf "Player %s won\n" (color_to_string winner)
    with NoWinningPlayer _ ->
      play ();
      aux ()
  in
  add_players player_lst;
  Board.start_game ();
  aux ()