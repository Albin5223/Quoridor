(*Specification of the structure of the game board and its characteristics; functions related to movement verification, display, etc.*)
open Types
open Printf

let boardSize = 17 (*size of the board in length and width*)
let wallStart = 10 (*Number of walls per player at the start of the game*)

let board = List.init boardSize (fun _ -> List.init boardSize (fun _ -> Empty)) (*board initialization*)

let playerTest = {position = (0,0); walls_left = wallStart; color = White}  (*Initializing a test player (to be removed later)*)

let game = { players = []; board = board; current_player = playerTest; game_state = Ingame} (*initialization of the game (to be modified later)*)
  
let isValidPosition pos = let (x, y) = pos in (*Tests if the given position is valid*)
  if x >= 0 && x < boardSize && y >= 0 && y < boardSize then true else false

let getCellContent pos =
  let (x, y) = pos in
  if isValidPosition pos then
    List.nth (List.nth board y) x
  else
    raise (OutOfBounds "Position is outside the board boundaries")
  
let isWall = function  (*Test if the cell contains a wall*)
  | Wall -> true
  | _ -> false

let isPlayer = function (* Test if cell contains a player *)
  | Player _ -> true
  | _ -> false

let isWallPosition pos = (*Tests if the given position can contain a wall*)
  let (x, y) = pos in 
  x mod 2 = 1 || y mod 2 = 1

let getX position = fst position
let getY position = snd position
  
let listOfWalls pos = (*Returns a list of valid movements according to the positions of the walls around*)
  let (x, y) = pos in

  if not (isValidPosition pos) then 
    raise (OutOfBounds "Position is outside the board boundaries");

  if isWallPosition pos then 
    raise (InvalidWallPosition "Given position is a wall position");

  let checkWallAtPosition (dx, dy) =
    let newPos = (x + dx, y + dy) in
    if isValidPosition newPos && isWall (getCellContent newPos) then
      [newPos]
    else
      []
  in

  List.flatten (List.map checkWallAtPosition [(-1, 0); (1, 0); (0, -1); (0, 1)])


let listOfMoves pos =
  let (x, y) = pos in

  if not (isValidPosition pos) then 
    raise (OutOfBounds "Position is outside the board boundaries");

  if isWallPosition pos then 
    raise (InvalidWallPosition "Given position is a wall position");

  let wallsAround = listOfWalls pos in
  let checkMoveDirection (dx, dy) =
    let wallPos = (x + dx, y + dy) in
    let newPos = (x + 2 * dx, y + 2 * dy) in

    if List.mem wallPos wallsAround then 
      []
    else if isPlayer (getCellContent newPos) then
      let jumpWallPos = (x + 3 * dx, y + 3 * dy) in
      let finalJumpPos = (x + 4 * dx, y + 4 * dy) in
      if isWall (getCellContent jumpWallPos) || isPlayer (getCellContent finalJumpPos) || not (isValidPosition finalJumpPos) then
        let adjacentCells = [
          (x + 2 * dx + 2, y + 2 * dy); 
          (x + 2 * dx - 2, y + 2 * dy); 
          (x + 2 * dx, y + 2 * dy + 2); 
          (x + 2 * dx, y + 2 * dy - 2)
        ] in
        List.filter (fun pos -> 
            let (px, py) = pos in
            isValidPosition pos && 
            not (isWallPosition pos) &&
            not (isPlayer (getCellContent pos)) && 
            (px = x + 2 * dx || py = y + 2 * dy)
        ) adjacentCells
      else 
        [newPos; finalJumpPos]      
    else
      [newPos]
  in

  List.flatten (List.map checkMoveDirection [(-1, 0); (1, 0); (0, -1); (0, 1)])

let canPlaceHorizontalWall game pos =
  let (x, y) = pos in
  if not (isWallPosition (x, y) || isWallPosition (x + 1, y)) then
    false
  else
    not (isWall (getCellContent (x, y))) && 
    isValidPosition (x, y) && isValidPosition (x + 1, y) &&
    game.current_player.walls_left > 0

let canPlaceVerticalWall game pos =
  let (x, y) = pos in
  if not (isWallPosition (x, y) || isWallPosition (x, y + 1)) then
    false
  else
    not (isWall (getCellContent (x, y))) && 
    isValidPosition (x, y) && isValidPosition (x, y + 1) &&
    game.current_player.walls_left > 0
  
let placeHorizontalWall game pos =
  let (x, y) = pos in 
  if canPlaceHorizontalWall game pos then
    let updated_row1 = List.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (List.nth game.board y) in
    let updated_board = List.mapi (fun i row -> 
      if i = y then updated_row1 else row
    ) game.board in
    { game with board = updated_board; 
                current_player = { game.current_player with walls_left = game.current_player.walls_left - 1 } }
  else
    raise (InvalidWallPosition "Cannot place horizontal wall here")
  
let placeVerticalWall game pos =
  let (x, y) = pos in 
  if canPlaceVerticalWall game pos then
    let updated_row1 = List.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (List.nth game.board y) in
    let updated_row2 = List.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (List.nth game.board (y + 1)) in
    let updated_board = List.mapi (fun i row -> 
      if i = y then updated_row1 
      else if i = y+1 then updated_row2 
      else row
    ) game.board in
    { game with board = updated_board; 
                current_player = { game.current_player with walls_left = game.current_player.walls_left - 1 } }
  else
    raise (InvalidWallPosition "Cannot place vertical wall here")
    
