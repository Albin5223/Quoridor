open Types
open Board

let create_player pos walls_left color strat =
  { position = pos; walls_left; color; strategy = strat }

let init_game (attribut_lst : player list) =
  List.iter (fun pl -> Board.add_player_to_board pl) attribut_lst

let play () =
  let strat = (current_player ()).strategy in
  let pos = (current_player ()).position in
  let move = strat pos in
  match move with
  | Wall (pos1, pos2) -> Board.place_wall pos1 pos2
  | Moving pos -> Board.move_player pos

let print_color player =
  match player.color with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  | Yellow -> "yellow"

let run_game player_lst =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let winner = Board.winning_player () in
      Format.printf "Player %s won\n" (print_color winner)
    with NoWinningPlayer _ ->
      play ();
      aux ()
  in
  init_game player_lst;
  Board.start_game ();
  aux ()
