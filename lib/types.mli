(** Types.ml
    This module defines the basic types and data structures used in the Quoridor game.
*)

(** {1 Basic Types} *)

(** Color of a player. *)
type color = Red | Green | Blue | Yellow

type position = int * int
(** Position on the board represented as a coordinate (x, y). *)

(** {1 Game Components} *)

type player = {
  position : position;  (** Current position of the player on the board. *)
  walls_left : int;  (** Number of walls the player can still place. *)
  color : color;
}
(** Attributes of a player including the position on the board, remaining walls, and color. *)

(** Content of each cell on the board. *)
type cell_content =
  | Empty  (** Empty cell. *)
  | Wall  (** Cell with a wall. *)
  | Player of player  (** Cell occupied by a player. *)

type board = cell_content array array
(** Game board represented as a 2D array of cell contents. *)

(** Game state indicating whether the game is ongoing or over. *)
type state =
  | Ingame  (** Game is ongoing. *)
  | GameOver of player
      (** Game is over with the specified player as the winner. *)

type game = {
  players : player list;  (** List of players in the game. *)
  board : board;  (** Current game board. *)
  current_player : player;  (** Player whose turn it is. *)
  state : state;  (** Current state of the game. *)
  winner : player option;
      (** Winning player if the game is over, otherwise None. *)
}
(** Main game structure. *)

(** {1 Game Exceptions} *)

exception OutOfBounds of string
(** Exception raised when accessing outside the board dimensions. *)

exception InvalidWallPosition of string
(** Exception raised when trying to place a wall in an invalid position. *)

exception InvalidPlayerPosition of string
(** Exception raised when trying to move a player to an invalid position. *)

exception InvalidMove of string
(** Exception raised when making a move that's not allowed by game rules. *)

exception InvalidPosition of string
(** Exception raised when specifying an invalid board position. *)

exception InvalidWallPlacement of string
(** Exception raised when placing a wall that blocks all paths to goal. *)

exception InvalidNumberPlayer of string
(** Exception raised when the number of players is not between 2 and 4 inclusive. *)

exception NoWinningPlayer of string
(** Exception raised when no player has reached their target position. *)
