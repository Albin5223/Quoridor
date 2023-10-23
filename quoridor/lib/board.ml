(*Specification of the structure of the game board and its characteristics; functions related to movement verification, display, etc.*)
open Types

let boardSize = 17 (*size of the board in length and width*)
let wallStart = 10 (*Number of walls per player at the start of the game*)

let board =  (*board initialization*)
  let row = List.init boardSize (fun _ -> Empty) in
  List.init boardSize (fun _ -> row)


let playerTest = {position = (0,0); walls_left = wallStart; color = White}  (*Initializing a test player (to be removed later)*)

let game = { players = []; board = board; current_player = playerTest; game_state = Ingame} (*initialization of the game (to be modified later)*)
  
let getCellContent pos = let (x, y) = pos in (* Returns the contents of a cell*)
  try 
    match List.nth (List.nth board y) x with
    |content -> content
  with Failure _ -> raise (OutOfBounds "Position is outside the board boundaries") 
  
let isValidPosition pos = let (x, y) = pos in (*Tests if the given position is valid*)
  if x >= 0 && x < boardSize && y >= 0 && y < boardSize then true else false
  
let isWall = function  (*Test if the cell contains a wall*)
  |Wall -> true
  |_ -> false   

let isWallPosition pos = let (x, y) = pos in (*Tests if the given position can contain a wall*)
  isWall (getCellContent (x,y))
    


let getX position = fst position
let getY position = snd position

 
  
let lstOfMoves_wall pos = (*Returns a list of valid movements according to the positions of the walls around*)
  let (x, y) = pos in
  
    if not (isValidPosition (x, y)) then raise (OutOfBounds "Position is outside the board boundaries");
    if isWallPosition (x, y) then raise (InvalidWallPosition "Given position is a wall position");

        (* Check if there's a wall at the specific position *)
        let checkWallAtPosition (dx, dy) =
          if isValidPosition (x + dx, y + dy)  then 
            if isWall (getCellContent (x + dx, y + dy)) then [(x + dx, y + dy)] else []
          else raise (OutOfBounds "Position is outside the board boundaries")
        in

          (* Check all surrounding positions *)
          checkWallAtPosition (-1, 0) @ checkWallAtPosition (1, 0) @ 
          checkWallAtPosition (0, -1) @ checkWallAtPosition (0, 1)
  
let filter position = fun (a,b) -> a <> (getX position) && b <> (getY position) 



let rec lstOfMoves pos =  (*returns the list of possible moves depending on the position of a player*)
  let addMoves pl lst = if (pl <> game.current_player && (List.exists (filter pl.position) lst)) then lstOfMoves pl.position else [] in
    let lst = lstOfMoves_wall pos in 
      let rec checkPlayerPos players lst = match players with
          |pl :: l -> let lst = (addMoves pl lst) @ (List.filter (filter pl.position) lst) in checkPlayerPos l lst 
          |_ -> lst
        in checkPlayerPos game.players lst


