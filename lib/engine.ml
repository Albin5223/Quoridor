open Board
open Types

(** [winning_player game] returns the first player of the game
    who has reached their target zone, indicating that the game is finished.
    @param game is the current game
    @return the player who won the game
    @raise [NoWinningPlayer] if no player has reached their target zone *)
let winning_player game =
  (* Hashtable to associate colors with the conditions to check if they are in their target zones *)
  let colors_zones = Hashtbl.create 4 in
  Hashtbl.add colors_zones Red (fun _ y -> y = board_size - 1);
  Hashtbl.add colors_zones Green (fun x _ -> x = board_size - 1);
  Hashtbl.add colors_zones Blue (fun _ y -> y = 0);
  Hashtbl.add colors_zones Yellow (fun x _ -> x = 0);
  (* List of colors of the game *)
  let game_colors = List.map (fun player -> player.color) game.players in
  (* Function to check if a player has reached their target zone *)
  let player_reached_target player =
    Hashtbl.fold
      (fun k v acc ->
        acc
        ||
        if k <> player.color && List.mem k game_colors then v (fst player.position) (snd player.position)
        else false)
      colors_zones false
    (* Find the player who has reached their target zone.
       Raises Not_found with a descriptive error message if no player is found *)
  in

  try List.find player_reached_target game.players
  with Not_found ->
    raise (NoWinningPlayer "No player has reached their target zone")
