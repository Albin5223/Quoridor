(* Spécification des différents types du jeu (joueurs, plateau, murs, ...) *)

type color = Black | White

type position = XY of int * int

type player = {
  mutable position : position;
  color : color
}

type wall = position * position

type board = position list list

type state = Ingame | EndGame

type game = {
  players: player list;
  walls: wall list;
  board: board;
  current_player: player;
  game_state: state;
}
