type color = Red | Green | Blue | Yellow
type position = int * int
type player = { position : position; walls_left : int; color : color; }
type cell_content = Empty | Wall | Player of player
type board = cell_content array array
type state = Ingame | GameOver of player
type game = {
  players : player list;
  board : board;
  current_player : player;
  game_state : state;
}

type cell_content =
  | Empty
  | Wall
  | Player of player

type board = cell_content array array  (* Game board *)

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
