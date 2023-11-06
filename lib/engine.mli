(** engine.ml
    Implementation of the game's engine; handling initialization, state transitions,
    player actions, and victory conditions.
*)

(** {1 Initialisation Functions} *)

val init_board : Types.cell_content array array
(** [init_board] initializes a game board with all cells set to Empty.
    @return a board_size x board_size array with all cells set to Empty.
*)

val init_player : Types.position -> Types.color -> Types.player
(** [init_player pos color] initializes a player with a given position and color.
    @param pos the starting position of the player.
    @param color the color assigned to the player.
    @return a player record with the given position, 10 walls left, and specified color.
*)

val add_players_to_board :
  Types.cell_content array array ->
  Types.player list ->
  Types.cell_content array array
(** [add_players_to_board board players] adds players to the board.
    @param board the game board to add players to.
    @param players the list of players to be added to the board.
    @return a new board with players added to their starting positions.
*)

val init_game : int -> Types.game
(** [init_game nb_players] initializes the game with a specified number of players.
    @param nb_players the number of players (must be between 2 and 4 inclusive).
    @return the initial state of the game with players placed on the board and the first player set as the current player.
    @raise InvalidNumberPlayer if nb_players is not between 2 and 4.
*)

(** {1 Game State Update Functions} *)

val change_pos_of_player :
  Types.game -> Types.player -> Types.position -> Types.game
(** [change_pos_of_player game player pos] changes the position of a player on the board.
    @param game the current state of the game.
    @param player the player whose position is to be changed.
    @param pos the new position for the player.
    @return a new game state with the player moved to the new position.
    @raise Invalid_argument if the old or new position is out of bounds.
*)

val move : Types.game -> Types.player -> Types.game
(** [move game player] moves a player to a new position chosen at random from the list of valid moves.
    @param game the current state of the game.
    @param player the player to move.
    @return a new game state with the player moved to the new position.
*)

val place_wall_random : Types.game -> Types.player -> Types.game
(** [place_wall_random game player] attempts to place a wall at a random position.
    @param game the current state of the game.
    @param player the player attempting to place a wall.
    @return a new game state with the wall placed if successful.
    @raise InvalidWallPlacement if the wall placement is not possible.
*)

val det_move : Types.game -> Types.player -> Types.game
(** [det_move game player] determines and executes the next action for a player.
    @param game the current state of the game.
    @param player the player whose action is to be determined.
    @return a new game state after the player's action.
*)

val change_current_player : Types.game -> Types.game
(** [change_current_player game] updates the game state to set the next player as the current player.
    @param game the current state of the game.
    @return a new game state with the next player set as the current player.
*)

(** {1 Winning Condition Functions} *)

val winning_player : Types.game -> Types.player
(** [winning_player game] determines if there is a winning player who has reached their target zone.
    @param game the current state of the game.
    @return the player who has won the game.
    @raise NoWinningPlayer if no player has reached their target zone.
*)

(** {1 Game Execution Function} *)

val run_game : unit
(** [run_game] starts and manages the game loop until a player wins or the game ends.
*)
