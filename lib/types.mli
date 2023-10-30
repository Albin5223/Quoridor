type color = Red | Green | Blue | Yellow
type position = int * int
type player = { position : position; walls_left : int; color : color }
type cell_content = Empty | Wall | Player of player
type board = cell_content array array
type state = Ingame | GameOver of player

type game = {
  players : player list;
  board : board;
  current_player : player;
  game_state : state;
}

exception OutOfBounds of string
exception InvalidWallPosition of string
exception InvalidPlayerPosition of string
exception InvalidMove of string
exception InvalidPosition of string
exception InvalidWallPlacement of string
