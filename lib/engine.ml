open Board
open Types

let move_vectors = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

let list_of_walls pos =
  let x, y = pos in

  if not (Board.is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + dx, y + dy) in
      if Board.is_valid_position newPos && Board.is_wall newPos then
        newPos :: acc
      else acc)
    [] move_vectors

let list_of_players pos =
  let x, y = pos in

  if not (Board.is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

  List.fold_left
    (fun acc (dx, dy) ->
      let newPos = (x + (2 * dx), y + (2 * dy)) in
      if Board.is_valid_position newPos && Board.is_player newPos then
        newPos :: acc
      else acc)
    [] move_vectors

let list_of_moves pos =
  let x, y = pos in

  if not (Board.is_valid_position (x, y)) then
    raise (OutOfBounds "Position is outside the board boundaries");
  if not (Board.is_player_position pos) then
    raise (InvalidPlayerPosition "Given position is not a player's position");

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
        if
          Board.is_valid_position jumpPos
          && (not (Board.is_player jumpPos))
          && not (Board.is_wall_between newPos jumpPos)
        then jumpPos :: acc
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
                if
                  Board.is_valid_position pos
                  && (not (Board.is_player pos))
                  && not (Board.is_wall_between newPos pos)
                then pos :: acc
                else acc)
              [] adjacent_positions_around_newPos
          in
          List.append acc valid_adjacent_positions
        (* Ajoute le mouvement à la liste s'il n'y a ni mur ni joueur bloquant la direction. *)
      else if Board.is_valid_position newPos && not (Board.is_player newPos)
      then newPos :: acc
      else acc)
    [] move_vectors

let dfs_path_exists player pos1 pos2 =
  let start_pos = player.position in

  if not (Board.is_valid_position start_pos) then
    raise (OutOfBounds "Start position is outside the board boundaries");
  if not (Board.is_player_position start_pos) then
    raise (InvalidPlayerPosition "Start position is not a player's position");

  (* Création d'une matrice pour suivre les positions visitées *)
  let visited = Array.make_matrix Board.board_size Board.board_size false in

  let is_target_position pos =
    let x, y = pos in
    match player.color with
    | Blue -> y = 0
    | Red -> y = Board.board_size - 1
    | Yellow -> x = 0
    | Green -> x = Board.board_size - 1
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
          else if
            Board.is_valid_position next_pos && not visited.(next_y).(next_x)
          then dfs next_pos
          else false)
        next_moves)
  in
  dfs start_pos

let place_wall pos1 pos2 players =
  if not (Board.is_valid_position pos1 && Board.is_valid_position pos2) then
    raise (OutOfBounds "Position is outside the board boundaries");
  if not (Board.is_wall_position pos1 && Board.is_wall_position pos2) then
    raise (InvalidWallPosition "Given position is not a wall position");

  if Board.is_wall pos1 || Board.is_wall pos2 then
    raise (InvalidWallPlacement "A wall already exists at this position");

  (* On vérifie si le placement est possible et que le mur posé n'est pas bloquant,
     sinon on annule le placement et on lève une exception *)
  if List.for_all (fun player -> dfs_path_exists player pos1 pos2) players then
    Board.place_wall pos1 pos2
  else
    raise (InvalidWallPlacement "Wall placement blocks a player's path to goal")

let init_player pos color = { position = pos; walls_left = 10; color }

let init_game nb_players =
  (* Détermine les positions initiales des joueurs en fonction du nombre de joueurs *)
  let player_positions =
    match nb_players with
    | 2 ->
        [|
          (Board.board_size / 2, 0); (Board.board_size / 2, Board.board_size - 1);
        |]
    | 3 ->
        [|
          (Board.board_size / 2, 0);
          (Board.board_size / 2, Board.board_size - 1);
          (0, Board.board_size / 2);
        |]
    | 4 ->
        [|
          (Board.board_size / 2, 0);
          (Board.board_size / 2, Board.board_size - 1);
          (0, Board.board_size / 2);
          (Board.board_size - 1, Board.board_size / 2);
        |]
    | _ ->
        raise
          (InvalidNumberPlayer
             "the number of players must be between 2 and 4 inclusive")
  in
  (* Création de la liste des joueurs avec leurs couleurs et positions initiales *)
  let color_list = [| Red; Blue; Green; Yellow |] in
  let players_list =
    List.init nb_players (fun i ->
        init_player player_positions.(i) color_list.(i))
  in
  add_players_to_board players_list;

  let current_player = List.nth players_list 0 in
  { players = players_list; current_player; state = Ingame; winner = None }

let change_pos_of_player game player pos =
  Board.update_player_position player pos;
  let new_player = { player with position = pos } in
  let new_lst_players =
    new_player :: List.filter (fun pl -> pl <> player) game.players
  in
  { game with players = new_lst_players; current_player = new_player }

let random_move game player =
  let lstMv = list_of_moves player.position in
  match lstMv with
  | [] -> game
  | _ ->
      let r = Random.int (List.length lstMv) in
      let newPos = List.nth lstMv r in
      change_pos_of_player game player newPos

let rec place_wall_random game player =
  let rec generate_random_wall_pos () =
    let x1 = Random.int Board.board_size in
    let y1 = Random.int Board.board_size in
    let r = Random.int 4 in
    let xv, yv = List.nth move_vectors r in
    Format.printf "%d,%d " x1 y1;
    try
      if
        Board.is_wall_position (x1, y1)
        && Board.is_wall_position (x1 + xv, y1 + yv)
      then ((x1, y1), (x1 + xv, y1 + yv))
      else generate_random_wall_pos ()
    with InvalidPosition _ -> generate_random_wall_pos ()
  in
  let wall_pos1, wall_pos2 = generate_random_wall_pos () in
  Format.printf "\n\nPlacing wall on pos1 (%d, %d) & pos2 (%d, %d)\n\n"
    (fst wall_pos1) (snd wall_pos1) (fst wall_pos2) (snd wall_pos2);

  try
    place_wall wall_pos1 wall_pos2 game.players;
    let new_player = { player with walls_left = player.walls_left - 1 } in
    let new_lst_players =
      new_player :: List.filter (fun pl -> pl <> player) game.players
    in
    { game with players = new_lst_players; current_player = new_player }
  with InvalidWallPlacement _ -> place_wall_random game player

let det_move game player =
  let r = Random.int 3 in
  if r == 0 && player.walls_left > 0 then place_wall_random game player
  else random_move game player

let change_current_player game =
  let current_player = game.current_player in
  let new_player_lst =
    List.filter (fun p -> p <> current_player) game.players @ [ current_player ]
  in
  {
    game with
    players = new_player_lst;
    current_player = List.nth new_player_lst 0;
  }

let winning_player game =
  (* Hashtable to associate colors with the conditions to check if they are in their target zones *)
  let colors_zones = Hashtbl.create 4 in
  Hashtbl.add colors_zones Red (fun _ y -> y = Board.board_size - 1);
  Hashtbl.add colors_zones Green (fun x _ -> x = Board.board_size - 1);
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

  try List.find player_reached_target game.players
  with Not_found ->
    raise (NoWinningPlayer "No player has reached their target zone")

let run_game =
  Random.self_init ();
  let rec aux game =
    Board.print_board ();
    if game.state = Ingame then
      let game = det_move game game.current_player in
      let game = change_current_player game in
      try
        let winner = winning_player game in
        aux
          {
            game with
            state = GameOver game.current_player;
            winner = Some winner;
          }
      with NoWinningPlayer _ -> aux game
    else Format.printf "partie terminée\n"
  in

  aux (init_game 4)
