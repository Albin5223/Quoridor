type color = Red | Green | Blue | Yellow
type position = int * int
type player = { position : position; walls_left : int; color : color }
type state = Ingame | GameOver of player

type game = {
  players : player list;
  current_player : player;
  state : state;
  winner : player option;
}

exception OutOfBounds of string
exception InvalidWallPosition of string
exception InvalidPlayerPosition of string
exception InvalidMove of string
exception InvalidPosition of string
exception InvalidWallPlacement of string
exception InvalidNumberPlayer of string
exception NoWinningPlayer of string
