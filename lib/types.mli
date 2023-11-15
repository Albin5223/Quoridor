(** Represents the color of a player. *)
type color = Red | Green | Blue | Yellow

type position = int * int
type move = Wall of position * position | Moving of position
type strategy = position -> move

type player_attribut = color * position * strategy
(** Represents a position on the game board as a pair of integers. *)

(** Represents the status of the game. *)
type game_status =
  | WaitingToStart  (** Game is initialized but not started. *)
  | InProgress  (** Game is currently in progress. *)
  | Finished of color option
      (** Game is finished. Option holds the winning color if there is one. *)

type player = {
  position : position;
  walls_left : int;
  color : color;
  strategy : strategy;
}

exception InvalidWallPosition of position * position * string
(** Raised when a wall is placed in an invalid position. *)

exception InvalidPlayerPosition of position * string
(** Raised when a player is moved to an invalid position. *)

exception InvalidMove of string
(** Raised when a move made in the game is invalid. *)

exception InvalidPosition of position * string
(** Raised when a specified position is invalid for the current game context. *)

exception InvalidPositionPair of position * position * string
(** Raised when a pair of positions are invalid in the given context. *)

exception InvalidWallPlacement of position * position * string
(** Raised when a wall is placed incorrectly on the game board. *)

exception InvalidNumberPlayer of int * string
(** Raised when the number of players in the game is invalid. *)

exception InvalidPlayerColor of color * string
(** Raised when an invalid color is assigned to a player. *)

exception NoWinningPlayer of string
(** Raised when there is no winning player in a game scenario where one is expected. *)

exception NoPlayersInGame

exception NoMove of string
(** Raised when an operation is attempted on a game with no players. *)

exception InvalidGameState of string
(** Raised when the game state is invalid for the attempted operation. *)
