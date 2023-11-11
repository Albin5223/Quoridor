open Types

type cell_content = Empty | Wall | Player of player
type board = cell_content array array

let board_size = 17
let game_board = Array.make_matrix board_size board_size Empty

let is_valid_position pos =
  let x, y = pos in
  x >= 0 && x < board_size && y >= 0 && y < board_size

let is_wall_position pos =
  if not (is_valid_position pos) then
    raise (InvalidPosition "Position is not valid on the board");

  let x, y = pos in
  (x mod 2 = 1 && y mod 2 = 0) || y mod 2 = 1

let is_player_position pos =
  if not (is_valid_position pos) then
    raise (InvalidPosition "Position is not valid on the board");

  let x, y = pos in
  x mod 2 = 0 && y mod 2 = 0

let get_cell_content pos =
  if not (is_valid_position pos) then
    raise (OutOfBounds "Position is outside the board boundaries");
  let x, y = pos in
  game_board.(y).(x)

let is_wall pos = match get_cell_content pos with Wall -> true | _ -> false

let is_player pos =
  match get_cell_content pos with Player _ -> true | _ -> false

let is_wall_between pos1 pos2 =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if pos1 = pos2 then raise (InvalidPosition "The two positions are the same");

  if not (is_valid_position pos1 && is_valid_position pos2) then
    raise (OutOfBounds "One of the positions is outside the board boundaries");

  if not (is_player_position pos1 && is_player_position pos2) then
    raise (InvalidPosition "Positions must be even coordinates");

  (* Vérification si les positions sont adjacentes et si un mur est présent *)
  if (abs (x1 - x2) = 2 && y1 = y2) || (x1 = x2 && abs (y1 - y2) = 2) then
    let wall_pos = ((x1 + x2) / 2, (y1 + y2) / 2) in
    is_wall_position wall_pos && is_wall wall_pos
  else raise (InvalidPosition "The positions are not adjacent")

let add_players_to_board players =
  List.iter
    (fun p ->
      let x, y = p.position in
      if not (is_valid_position p.position) then
        raise (InvalidPlayerPosition "Player position is out of bounds");
      if is_player p.position then
        raise (InvalidPlayerPosition "Player position is already occupied");
      game_board.(y).(x) <- Player p)
    players

let update_player_position player pos =
  let x, y = pos in
  let x_old, y_old = player.position in

  if not (is_valid_position pos) then
    raise (Invalid_argument "New position is out of bounds");

  if is_player pos then
    raise
      (InvalidPlayerPosition
         "New position is already occupied by another player");

  game_board.(y).(x) <- Player player;
  game_board.(y_old).(x_old) <- Empty

let place_wall pos1 pos2 =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if not (is_valid_position pos1 && is_valid_position pos2) then
    raise (InvalidWallPosition "Wall position is out of bounds");

  if is_wall pos1 || is_wall pos2 then
    raise (InvalidWallPlacement "Wall position is already occupied");

  game_board.(y1).(x1) <- Wall;
  game_board.(y2).(x2) <- Wall

let print_player p =
  match p.color with
  | Red -> Format.printf " R "
  | Green -> Format.printf " G "
  | Blue -> Format.printf " B "
  | Yellow -> Format.printf " Y "

let print_cell cell =
  match cell with
  | Player p -> print_player p
  | Wall -> Format.printf " # "
  | Empty -> Format.printf " . "

let print_row row = Array.iter (fun cell -> print_cell cell) row

let print_board () =
  Format.printf "@.";
  Array.iter
    (fun row ->
      print_row row;
      Format.printf "@;")
    game_board
