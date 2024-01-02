val det_move_gabin_bot : Types.strategy
(** [det_move_gabin_bot ()] Gabin_bot strategy, combining shortest-path movements and randomly placing
    a wall around a player to win. Can be used in both 1v1 and 1v1v1v1. **)

val choose_move_gabin_bot : Types.position -> Types.position
(** [choose_move_gabin_bot pos] From a given player's position [pos], calculate the shortest route to get there. **)

val choose_wall_gabin_bot :
  Types.position list -> (Types.position * Types.position) option
(** [choose_wall_gabin_bot pos_list] with the positions of the players to be attacked 
    [pos_list], return the wall position randomly around if any have been found, and if
    this wall has been chosen randomly. **)
