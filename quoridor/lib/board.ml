(* Spécification de la structure du plateau de jeu et de ses caractéristiques; fonctions liées à la vérification des déplacements, à l'affichage... *)
open Types

let boardSize = 17

let initBoard () = 
  let row = List.init boardSize (fun _ -> None) in
  List.init boardSize (fun _ -> row)

let getCellContent board (x, y) = 
  try 
    match List.nth (List.nth board y) x with
    | Some content -> content
    | None -> raise (OutOfBounds "Invalid cell content")
  with Failure _ -> raise (OutOfBounds "Position is outside the board boundaries")  

let getCoordinates = function 
| XY (x, y) -> 
  if x >= 0 && x < boardSize && y >= 0 && y < boardSize then (x, y)
  else raise (OutOfBounds "Position is outside the board boundaries")

let getX position = fst (getCoordinates position)
let getY position = snd (getCoordinates position)

let isValidPosition (x, y) =
  if x >= 0 && x < boardSize && y >= 0 && y < boardSize then true else false

let isWallPosition (x, y) =
  (x mod 2 = 0 || y mod 2 = 0) && (x mod 2 <> y mod 2)

let isWallAdjacent position walls =
  let (x, y) = getCoordinates position in
  
    if not (isValidPosition (x, y)) then raise (OutOfBounds "Position is outside the board boundaries");
    if isWallPosition (x, y) then raise (InvalidWallPosition "Given position is a wall position");

    (* Helper function to determine if a position is part of a wall *)
    let isPartOfWall pos wall =
      let (wall_start, wall_end) = wall in
        pos = getCoordinates wall_start || pos = getCoordinates wall_end
      in

        (* Check if there's a wall at the specific position *)
        let checkWallAtPosition (dx, dy) =
          isValidPosition (x + dx, y + dy) && List.exists (fun wall -> isPartOfWall (x + dx, y + dy) wall) walls
        in

          (* Check all surrounding positions *)
          checkWallAtPosition (-1, 0) || checkWallAtPosition (1, 0) || 
          checkWallAtPosition (0, -1) || checkWallAtPosition (0, 1)
  

(*
let lstOfMoves pl1 pl2 = 
  definir selon si il y a a coté un/une:
  - mur
  -extremite
  -joueur
  -rien
  *)
