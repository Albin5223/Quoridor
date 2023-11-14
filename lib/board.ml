open Types

type player = { position : position; walls_left : int; color : color }
type cell_content = Empty | Wall | Player of player
type board = cell_content array array
type state = { mutable players : player list }

let board_size = 17
let game_board = Array.make_matrix board_size board_size Empty
let move_vectors = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
let game_state = { players = [] }
let current_player () = List.hd game_state.players

let update_player_order () =
  match game_state.players with
  | [] -> ()
  | first :: rest -> game_state.players <- rest @ [ first ]

let pos_current_player () = (current_player ()).position
let walls_left_current_player () = (current_player ()).walls_left

let validate_position pos =
  let x, y = pos in
  if not (x >= 0 && x < board_size && y >= 0 && y < board_size) then
    raise (InvalidPosition (pos, "Position is outside the board boundaries"))

let is_wall_position pos =
  validate_position pos;

  let x, y = pos in
  (x mod 2 = 1 && y mod 2 = 0) || y mod 2 = 1

let is_player_position pos =
  validate_position pos;

  let x, y = pos in
  x mod 2 = 0 && y mod 2 = 0

let get_cell_content pos =
  validate_position pos;

  let x, y = pos in
  game_board.(y).(x)

let is_wall pos = match get_cell_content pos with Wall -> true | _ -> false

let is_player pos =
  match get_cell_content pos with Player _ -> true | _ -> false

let is_wall_between pos1 pos2 =
  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if pos1 = pos2 then
    raise (InvalidPositionPair (pos1, pos2, "The two positions are the same"));

  validate_position pos1;
  validate_position pos2;

  if not (is_player_position pos1 && is_player_position pos2) then
    raise
      (InvalidPositionPair (pos1, pos2, "Positions must be even coordinates"));

  (* Vérification si les positions sont adjacentes et si un mur est présent *)
  if (abs (x1 - x2) = 2 && y1 = y2) || (x1 = x2 && abs (y1 - y2) = 2) then
    let wall_pos = ((x1 + x2) / 2, (y1 + y2) / 2) in
    is_wall wall_pos
  else
    raise (InvalidPositionPair (pos1, pos2, "The positions are not adjacent"))

let list_of_walls pos =
  let x, y = pos in

  if not (is_player_position pos) then
    raise
      (InvalidPlayerPosition (pos, "Given position is not a player's position"));

  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + dx, y + dy) in
      try if is_wall newPos then newPos :: acc else acc
      with InvalidPosition _ -> acc)
    [] move_vectors

let list_of_players pos =
  let x, y = pos in

  if not (is_player_position pos) then
    raise
      (InvalidPlayerPosition (pos, "Given position is not a player's position"));

  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + (2 * dx), y + (2 * dy)) in
      try if is_player newPos then newPos :: acc else acc
      with InvalidPosition _ -> acc)
    [] move_vectors

let list_of_moves pos =
  let x, y = pos in

  if not (is_player_position pos) then
    raise
      (InvalidPlayerPosition (pos, "Given position is not a player's position"));

  let wallsAround = list_of_walls pos in
  let playersAround = list_of_players pos in

  (* Itération sur les vecteurs de mouvement possibles pour déterminer l'ensemble des mouvements valides.
      Cette fonction accumule une liste de mouvements en examinant chaque mouvement potentiel
      par rapport aux murs et aux autres joueurs. *)
  List.fold_left
    (fun acc (dx, dy) ->
      let wallPos = (x + dx, y + dy) in
      let newPos = (x + (2 * dx), y + (2 * dy)) in

      (* Bloque la direction de mouvement s'il y a un mur sur le chemin.
          Si un mur bloque la direction, le mouvement dans cette direction n'est pas ajouté à la liste. *)
      if List.exists (( = ) wallPos) wallsAround then acc
        (* Gère les cas où il y a un joueur dans la cellule adjacente.
            Si un joueur est dans la cellule adjacente, vérifie si le mouvement par-dessus le joueur est possible. *)
      else if List.exists (( = ) newPos) playersAround then
        let jumpPos = (x + (4 * dx), y + (4 * dy)) in

        let valid_jump () =
          try (not (is_player jumpPos)) && not (is_wall_between newPos jumpPos)
          with InvalidPosition _ | InvalidPositionPair _ -> false
        in

        if valid_jump () then jumpPos :: acc
        else
          (* Vérifie les mouvements valides autour du joueur obstruant *)
          let adjacent_positions_around_newPos =
            List.map
              (fun (ddx, ddy) ->
                let newX, newY = newPos in
                (* Position du joueur obstruant.
                    Calcule les positions adjacentes autour de ce joueur en utilisant les vecteurs de mouvement. *)
                (newX + (2 * ddx), newY + (2 * ddy)))
              move_vectors
          in
          let valid_adjacent_positions =
            List.fold_left
              (fun acc pos ->
                try
                  if (not (is_player pos)) && not (is_wall_between newPos pos)
                  then pos :: acc
                  else acc
                with InvalidPosition _ | InvalidPositionPair _ -> acc)
              [] adjacent_positions_around_newPos
          in
          List.append acc valid_adjacent_positions
        (* Ajoute le mouvement à la liste s'il n'y a ni mur ni joueur bloquant la direction. *)
      else
        try if not (is_player newPos) then newPos :: acc else acc
        with InvalidPosition _ -> acc)
    [] move_vectors

let dfs_path_exists player pos1 pos2 =
  let start_pos = player.position in

  if not (is_player_position start_pos) then
    raise
      (InvalidPlayerPosition
         (start_pos, "Start position is not a player's position"));

  (* Création d'une matrice pour suivre les positions visitées *)
  let visited = Array.make_matrix board_size board_size false in

  let is_target_position pos =
    let x, y = pos in
    match player.color with
    | Blue -> y = 0
    | Red -> y = board_size - 1
    | Yellow -> x = 0
    | Green -> x = board_size - 1
  in

  let rec dfs pos =
    let x, y = pos in
    if is_target_position pos then true
    else if visited.(y).(x) then false
    else (
      visited.(y).(x) <- true;
      let next_moves = list_of_moves pos in

      List.exists
        (fun next_pos ->
          let next_x, next_y = next_pos in
          (* Exclut les mouvements vers les positions exclues *)
          if next_pos = pos1 || next_pos = pos2 then false
          else if not visited.(next_y).(next_x) then dfs next_pos
          else false)
        next_moves)
  in
  dfs start_pos

let validate_wall_placement player pos1 pos2 =
  if player.walls_left <= 0 then
    raise
      (InvalidWallPlacement (pos1, pos2, "Player has no walls left to place"));

  if not (is_wall_position pos1 && is_wall_position pos2) then
    raise
      (InvalidWallPosition (pos1, pos2, "Given position is not a wall position"));

  if is_wall pos1 || is_wall pos2 then
    raise
      (InvalidWallPlacement
         (pos1, pos2, "A wall already exists at this position"));

  let x1, y1 = pos1 in
  let x2, y2 = pos2 in

  if not ((x1 = x2 && abs (y1 - y2) = 1) || (y1 = y2 && abs (x1 - x2) = 1)) then
    raise
      (InvalidWallPosition
         (pos1, pos2, "Wall positions must be adjacent and aligned"));

  if
    List.exists
      (fun player -> not (dfs_path_exists player pos1 pos2))
      game_state.players
  then
    raise
      (InvalidWallPlacement
         (pos1, pos2, "Wall placement blocks a player's path to goal"))

let place_wall pos1 pos2 =
  let player = current_player () in
  validate_wall_placement player pos1 pos2;

  let x1, y1 = pos1 in
  let x2, y2 = pos2 in
  game_board.(y1).(x1) <- Wall;
  game_board.(y2).(x2) <- Wall;

  let updated_player = { player with walls_left = player.walls_left - 1 } in
  game_state.players <-
    List.map
      (fun p -> if p = player then updated_player else p)
      game_state.players;

  let x, y = player.position in
  game_board.(y).(x) <- Player updated_player;

  update_player_order ()

let move_player pos2 =
  let current_pos = (current_player ()).position in
  if not (List.exists (( = ) pos2) (list_of_moves current_pos)) then
    raise
      (InvalidMove
         "The target position is not reachable from the current position")
  else
    let x, y = current_pos in
    game_board.(y).(x) <- Empty;

    let updated_player = { (current_player ()) with position = pos2 } in
    game_state.players <-
      List.map
        (fun p -> if p = current_player () then updated_player else p)
        game_state.players;

    let new_x, new_y = pos2 in
    game_board.(new_y).(new_x) <- Player updated_player;

    update_player_order ()

let is_border_position pos =
  let x, y = pos in
  let middle = board_size / 2 in
  (x = middle && (y = 0 || y = board_size - 1))
  || (y = middle && (x = 0 || x = board_size - 1))

let add_player_to_board color pos =
  let current_players = game_state.players in
  let nbPlayers = List.length current_players in
  if nbPlayers >= 4 then
    raise
      (InvalidNumberPlayer
         (nbPlayers, "Cannot have more than 4 players on the board"));

  if List.exists (fun p -> p.color = color) current_players then
    raise
      (InvalidPlayerColor (color, "A player with the same color already exists"));

  let x, y = pos in
  if is_player pos then
    raise (InvalidPlayerPosition (pos, "Player position is already occupied"));
  if not (is_border_position pos) then
    raise (InvalidPlayerPosition (pos, "Player must be placed on a border"));

  let player = { position = pos; walls_left = 10; color } in
  game_board.(y).(x) <- Player player;
  game_state.players <- game_state.players @ [ player ]

let winning_player () =
  (* Hashtable to associate colors with the conditions to check if they are in their target zones *)
  let colors_zones = Hashtbl.create 4 in
  Hashtbl.add colors_zones Red (fun _ y -> y = board_size - 1);
  Hashtbl.add colors_zones Green (fun x _ -> x = board_size - 1);
  Hashtbl.add colors_zones Blue (fun _ y -> y = 0);
  Hashtbl.add colors_zones Yellow (fun x _ -> x = 0);

  (* Function to check if a player has reached their target zone *)
  let player_reached_target player =
    Hashtbl.fold
      (fun k v acc ->
        acc
        ||
        if k = player.color then v (fst player.position) (snd player.position)
        else false)
      colors_zones false
    (* Find the player who has reached their target zone.
        Raises Not_found with a descriptive error message if no player is found *)
  in

  try List.find player_reached_target game_state.players
  with Not_found ->
    raise (NoWinningPlayer "No player has reached their target zone")

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
