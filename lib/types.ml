type color = Red | Green | Blue | Yellow
type position = int * int
type game_status = WaitingToStart | InProgress | Finished of color option

exception InvalidWallPosition of position * position * string
exception InvalidPlayerPosition of position * string
exception InvalidMove of string
exception InvalidPosition of position * string
exception InvalidPositionPair of position * position * string
exception InvalidWallPlacement of position * position * string
exception InvalidNumberPlayer of int * string
exception InvalidPlayerColor of color * string
exception NoWinningPlayer of string
exception NoPlayersInGame
exception InvalidGameState of string
