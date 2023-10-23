(* Specification of the different types for the game (players, board, walls, ...) *)

type color = Black | White  (* Player's color *)

type position = int * int  (* Board position *)

type player = {  (* Player's attributes *)
  mutable position : position;   (* Player's current position *)
  mutable walls_left : int;      (* Remaining walls available to the player *)
  color : color                  (* Player's color *)
}

type cell_content =
  | Empty
  | Wall
  | Player of player

type board = cell_content list list  (* Game board *)

type state = Ingame | EndGame of player  (* Game's state: ongoing or finished with a winner *)

type game = {  (* Game's attributes *)
  players: player list;          (* List of players *)
  board: board;                  (* Game board *)
  current_player: player;        (* Player whose turn it is *)
  game_state: state;             (* Current game state *)
}

exception OutOfBounds of string           (* Raised when a position is outside the board *)
exception InvalidWallPosition of string   (* Raised for walls in invalid positions *)
exception InvalidMove of string           (* Raised when a move is illegal, i.e. out of bounds, on other player / wall... *)