val create_player :
  Types.position -> int -> Types.color -> Types.strategy -> Types.player
(** Return a new player with the specified attributes
    @param pos The starting position of the player on the game board.
    @param walls_left The number of walls remaining of the player;
    @param color The color associated with the player.
    @param strat The strategy that the player will use during the game.
*)

val add_players : Types.player list -> unit
(** Adds each player from the given list to the game board
    @param player_lst The list of players in the game.
*)

val run_game : Types.player list -> unit
(** Starts and manages the game loop until a player wins or the game ends.
    @param player_lst The list of players in the game.
*)
