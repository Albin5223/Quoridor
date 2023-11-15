type color = Red | Green | Blue | Yellow
type position = int * int
type move = Wall of position * position | Moving of position
type strategy = position -> move

type player = {
  position : position;
  walls_left : int;
  color : color;
  strategy : strategy;
}

type game_status = WaitingToStart | InProgress | Finished of player option

exception InvalidWallPosition of position * position * string
exception InvalidPlayerPosition of position * string
exception InvalidMove of string
exception InvalidPosition of position * string
exception InvalidPositionPair of position * position * string
exception InvalidWallPlacement of position * position * string
exception InvalidNumberPlayer of int * string
exception InvalidPlayerColor of color * string
exception InvalidPlayerWallsLeft of int * string
exception NoWinningPlayer of string
exception NoPlayersInGame
exception NoMovePossible of string
exception InvalidGameState of string
