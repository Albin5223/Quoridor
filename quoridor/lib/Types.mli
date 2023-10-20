type  color = Black | White
type position = XY of int * int
type player = {
  mutable position : position;
  color : color
}

type wall = position * position
type board = position list

type state = Ingame | EndGame