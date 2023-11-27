type color = Red | Green | Blue | Yellow
type position = int * int
type move = Placing_wall of position * position | Moving of position
type strategy = position -> move

type player = {
  start_position : position;
  current_position : position;
  walls_left : int;
  color : color;
  strategy : strategy;
}

type game_status = WaitingToStart | InProgress | Finished of player

exception InvalidWallPosition of position * position * string
exception InvalidPlayerPosition of position * string
exception InvalidMove of string
exception InvalidPosition of position * string
exception InvalidPositionPair of position * position * string
exception InvalidWallPlacement of position * position * string
exception InvalidNumberPlayer of int * string
exception InvalidPlayerColor of color * string
exception InvalidPlayerWallsLeft of string
exception NoWinningPlayer of string
exception NoPlayersInGame
exception NoMovePossible of string
exception InvalidGameState of string
