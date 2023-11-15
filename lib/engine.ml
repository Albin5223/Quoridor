open Types
open Board

let init_game (attribut_lst : player_attribut list) =
  List.iter
    (fun el ->
      let col, pos, strat = el in
      Board.add_player_to_board col pos strat)
    attribut_lst

let play () =
  let strat = strategy_current_player () in
  let pos = pos_current_player () in
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

let run_game attribut_lst =
  Random.self_init ();
  let rec aux () =
    Board.print_board ();
    try
      let winner = Board.winning_player () in
      Format.printf "Player %s win\n" (print_color winner)
    with NoWinningPlayer _ ->
      play ();
      aux ()
  in
  init_game attribut_lst;
  Board.start_game ();
  aux ()
