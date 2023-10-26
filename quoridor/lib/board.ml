(*Specification of the structure of the game board and its characteristics; functions related to movement verification, display, etc.*)
open Types

let board_size = 17 (*size of the board in length and width*)
let wall_start = 10 (*Number of walls per player at the start of the game*)

let board = Array.init board_size (fun _ -> Array.init board_size (fun _ -> Empty)) (*board initialization*)

let player_test = {position = (0,0); walls_left = wall_start; color = White}  (*Initializing a test player (to be removed later)*)

let game = { players = []; board = board; current_player = player_test; game_state = Ingame} (*initialization of the game (to be modified later)*)
  
let is_valid_position pos = let (x, y) = pos in (*Tests if the given position is valid*)
  if x >= 0 && x < board_size && y >= 0 && y < board_size then true else false

let get_cell_content pos =
  let (x, y) = pos in
  if is_valid_position pos then
    Array.get (Array.get board y) x
  else
    raise (OutOfBounds "Position is outside the board boundaries")
  
let is_wall = function  (*Test if the cell contains a wall*)
  | Wall -> true
  | _ -> false

let is_player = function (* Test if cell contains a player *)
  | Player _ -> true
  | _ -> false

let is_wall_position pos = (*Tests if the given position can contain a wall*)
  let (x, y) = pos in 
  x mod 2 = 1 || y mod 2 = 1

let getX position = fst position
let getY position = snd position
  
let list_of_wall pos = (*Returns a list of valid movements according to the positions of the walls around*)
  let (x, y) = pos in

  if not (is_valid_position pos) then 
    raise (OutOfBounds "Position is outside the board boundaries");

  if is_wall_position pos then 
    raise (InvalidWallPosition "Given position is a wall position");

  let check_wall_at_position (dx, dy) =
    let newPos = (x + dx, y + dy) in
    if is_valid_position newPos && is_wall (get_cell_content newPos) then
      [newPos]
    else
      []
  in

  List.flatten (List.map check_wall_at_position [(-1, 0); (1, 0); (0, -1); (0, 1)])


let list_of_moves pos =
  let (x, y) = pos in

  if not (is_valid_position pos) then 
    raise (OutOfBounds "Position is outside the board boundaries");

  if is_wall_position pos then 
    raise (InvalidWallPosition "Given position is a wall position");

  let walls_around = list_of_wall pos in
  let check_move_direction (dx, dy) =
    let wallPos = (x + dx, y + dy) in
    let newPos = (x + 2 * dx, y + 2 * dy) in

    if List.mem wallPos walls_around then 
      []
    else if is_player (get_cell_content newPos) then
      let jump_wall_pos = (x + 3 * dx, y + 3 * dy) in
      let final_jump_pos = (x + 4 * dx, y + 4 * dy) in
      if is_wall (get_cell_content jump_wall_pos) || is_player (get_cell_content final_jump_pos) || not (is_valid_position final_jump_pos) then
        let adjacentCells = [
          (x + 2 * dx + 2, y + 2 * dy); 
          (x + 2 * dx - 2, y + 2 * dy); 
          (x + 2 * dx, y + 2 * dy + 2); 
          (x + 2 * dx, y + 2 * dy - 2)
        ] in
        List.filter (fun pos -> 
            let (px, py) = pos in
            is_valid_position pos && 
            not (is_wall_position pos) &&
            not (is_player (get_cell_content pos)) && 
            (px = x + 2 * dx || py = y + 2 * dy)
        ) adjacentCells
      else 
        [final_jump_pos]      
    else
      [newPos]
  in

  List.flatten (List.map check_move_direction [(-1, 0); (1, 0); (0, -1); (0, 1)])

let can_place_horizontal_wall game pos =
  let (x, y) = pos in
  if not (is_wall_position (x, y) || is_wall_position (x + 1, y)) then
    false
  else
    not (is_wall (get_cell_content (x, y))) && 
    is_valid_position (x, y) && is_valid_position (x + 1, y) &&
    game.current_player.walls_left > 0

let can_place_vertical_wall game pos =
  let (x, y) = pos in
  if not (is_wall_position (x, y) || is_wall_position (x, y + 1)) then
    false
  else
    not (is_wall (get_cell_content (x, y))) && 
    is_valid_position (x, y) && is_valid_position (x, y + 1) &&
    game.current_player.walls_left > 0
  
let place_horizontal_wall game pos =
  let (x, y) = pos in 
  if can_place_horizontal_wall game pos then
    let updated_row1 = Array.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (Array.get game.board y) in
    let updated_board = Array.mapi (fun i row -> 
      if i = y then updated_row1 else row
    ) game.board in
    { game with board = updated_board; (*faire une fonction*)
                current_player = { game.current_player with walls_left = game.current_player.walls_left - 1 } }
  else
    raise (InvalidWallPosition "Cannot place horizontal wall here")
  
let place_vertical_wall game pos =
  let (x, y) = pos in 
  if can_place_vertical_wall game pos then
    let updated_row1 = Array.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (Array.get game.board y) in
    let updated_row2 = Array.mapi (fun j cell -> 
      if j = x then Wall else cell
    ) (Array.get game.board (y + 1)) in
    let updated_board = Array.mapi (fun i row -> 
      if i = y then updated_row1 
      else if i = y+1 then updated_row2 
      else row
    ) game.board in
    { game with board = updated_board; 
                current_player = { game.current_player with walls_left = game.current_player.walls_left - 1 } }
  else
    raise (InvalidWallPosition "Cannot place vertical wall here")
  
    
let change_pos_of_player game pos = let (x,y) = pos in let newBoard = board in
  let player = game.current_player in 
    let () = Array.set (Array.get newBoard y) x (Player player) in 
      let (x,y) = player.position in 
        let () = Array.set (Array.get newBoard y) x Empty in
          { game with board = newBoard;
            current_player = { game.current_player with position = pos}}