open Quoridor.Board
open Quoridor.Engine

let () =
  let winner = run_game [ random_player; random_player ] in
  Format.printf "Winner: %s@." (color_to_string winner)
