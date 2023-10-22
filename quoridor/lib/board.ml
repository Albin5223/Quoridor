(* Spécification de la structure du plateau de jeu et de ses caractéristiques; fonctions liées à la vérification des déplacements, à l'affichage... *)
open Types

let boardSize = 17

let board = 
  let row = List.init boardSize (fun _ -> Empty) in
  List.init boardSize (fun _ -> row)

  
let getCellContent pos = let (x, y) = pos in
  try 
    match List.nth (List.nth board y) x with
    |content -> content
  with Failure _ -> raise (OutOfBounds "Position is outside the board boundaries") 
  
let isValidPosition pos = let (x, y) = pos in
  if x >= 0 && x < boardSize && y >= 0 && y < boardSize then true else false
  
let isWall = function 
  |Wall -> true
  |_ -> false   

let isWallPosition pos = let (x, y) = pos in
  isWall (getCellContent (x,y))
    


let getX position = fst position
let getY position = snd position

 
  
let isWallAdjacent pos =
  let (x, y) = pos in
  
    if not (isValidPosition (x, y)) then raise (OutOfBounds "Position is outside the board boundaries");
    if isWallPosition (x, y) then raise (InvalidWallPosition "Given position is a wall position");

        (* Check if there's a wall at the specific position *)
        let checkWallAtPosition (dx, dy) =
          isValidPosition (x + dx, y + dy) && isWall (getCellContent (x + dx, y + dy))
        in

          (* Check all surrounding positions *)
          checkWallAtPosition (-1, 0) || checkWallAtPosition (1, 0) || 
          checkWallAtPosition (0, -1) || checkWallAtPosition (0, 1)
  

(*
let lstOfMoves pl1 = 
  definir selon si il y a a coté un/une:
  - mur
  -extremite
  -joueur
  -rien
  *)
