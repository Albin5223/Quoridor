(* --- Basic Types --- *)

type color = Red | Green | Blue | Yellow

(* Coordinate (x, y) on the board. *)
type position = int * int

(* --- Game Components --- *)

(* Attributes of a player: position on the board, remaining walls, and color. *)
type player = {
  position : position; (* Current position of the player on the board *)
  walls_left : int; (* Number of walls the player can still place *)
  color : color;
}

(* Each cell on the board can be empty, contain a wall, or contain a player. *)
type cell_content = Empty | Wall | Player of player
type board = cell_content array array (* Game board *)
type state = Ingame | GameOver of player

type game = {
  players : player list;
  board : board;
  current_player : player;
  game_state : state;
}

(* --- Game Exceptions --- *)

exception
  OutOfBounds of string (* Raised when accessing outside the board dimensions *)

exception
  InvalidWallPosition of
    string (* Raised when trying to place a wall in an invalid position *)

exception
  InvalidPlayerPosition of
    string (* Raised when trying to move a player to an invalid position *)

exception
  InvalidMove of
    string (* Raised when making a move that's not allowed by game rules *)

exception
  InvalidPosition of
    string (* Raised when specifying an invalid board position *)

exception
  InvalidWallPlacement of
    string (* Raised when placing a wall that blocks all paths to goal *)

exception
  NoWinningPlayer of
    string (* Raised when no player has reached their target position *)
