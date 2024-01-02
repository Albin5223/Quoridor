open Board
open Types

let get_all_player_positions_except pos_player =
  let all_player_pos = get_all_player_pos () in
  let all_player_current_position =
    List.map (fun (_, current_position) -> current_position) all_player_pos
  in
  List.filter
    (fun current_position -> current_position <> pos_player)
    all_player_current_position

(* The winneable positions is the whole edge opposite the initial position, without the position of the other players *)
let get_winneable_positions pos_init =
  let x_init_pos, y_init_pos = pos_init in
  let rec aux_get_winneable_position x y acc =
    if x >= board_size || y >= board_size then acc
    else if x_init_pos = board_size / 2 then
      if is_player (x, y) then aux_get_winneable_position (x + 2) y acc
      else aux_get_winneable_position (x + 2) y ((x, y) :: acc)
    else if is_player (x, y) then aux_get_winneable_position x (y + 2) acc
    else aux_get_winneable_position x (y + 2) ((x, y) :: acc)
  in
  if x_init_pos = board_size / 2 then
    let new_y = if y_init_pos = 0 then board_size - 1 else 0 in
    aux_get_winneable_position 0 new_y []
  else
    let new_x = if x_init_pos = 0 then board_size - 1 else 0 in
    aux_get_winneable_position new_x 0 []

(* Reachable positions are all positions where a player can be, counting his position *)
let get_reachable_positions pos =
  if not (is_player pos) then failwith "It's not a player position"
  else
    let rec aux_get_possible_plating_position x y acc =
      if x >= board_size || y >= board_size then acc
      else if (not (is_player (x, y))) || (x, y) = pos then
        aux_get_possible_plating_position
          (if x >= board_size - 1 then 0 else x + 2)
          (if x >= board_size - 1 then y + 2 else y)
          ((x, y) :: acc)
      else
        aux_get_possible_plating_position
          (if x = board_size - 1 then 0 else x + 2)
          (if x = board_size - 1 then y + 2 else y)
          acc
    in
    aux_get_possible_plating_position 0 0 []

type graph = { size : int; adj : (position * position list) array }
(* A graph is a structure with [size] vertices, and adjacencies contained
   in an array with the possible positions for a player in the game accompanied by the
   moves that are possible from these same positions in their list *)

let rec create_adj lst acc_adj =
  match lst with
  | [] -> acc_adj
  | (x2, y2) :: tl2 -> create_adj tl2 ((x2, y2) :: acc_adj)

let init_graph pos =
  let reachable_pos = get_reachable_positions pos in
  let size = List.length reachable_pos in
  let g = Array.make size ((0, 0), []) in
  let rec aux_init_graph i pos_acc =
    match pos_acc with
    | [] -> g
    | (x, y) :: tl1 ->
        let lst_moves = list_of_moves (x, y) in
        g.(i) <- ((x, y), create_adj lst_moves []);
        aux_init_graph (i + 1) tl1
  in
  { size; adj = aux_init_graph 0 reachable_pos }

let find_opt p a =
  let rec aux_find_opt i =
    if i >= Array.length a then None
    else if p a.(i) then Some a.(i)
    else aux_find_opt (i + 1)
  in
  aux_find_opt 0

let get_adjacent_positions g pos =
  let pos_array = find_opt (fun (pos_a, _) -> pos_a = pos) g.adj in
  match pos_array with
  | None -> failwith "The given pos doesn't exist"
  | Some (_, lst) -> lst

type queue = (position * int) list
(* A queue is a list of pair of a position and distance *)

let get_min_distance_of_queue (q : queue) =
  if q = [] then failwith "Empty queue"
  else
    let rec aux_get_min q_acc min_pos min_value =
      match q_acc with
      | [] -> min_pos
      | (pos, d) :: tl ->
          if d < min_value then aux_get_min tl pos d
          else aux_get_min tl min_pos min_value
    in
    aux_get_min q (0, 0) max_int

let rec remove_queue q pos =
  let x, y = pos in
  match q with
  | [] ->
      Format.printf "(%d, %d)\n" x y;
      failwith "The given pos doesn't exist in this queue"
  | (pos_q, d_q) :: tl ->
      if pos = pos_q then tl else (pos_q, d_q) :: remove_queue tl pos

let rec dicrease_pos_queue q (x, y) new_value =
  match q with
  | [] -> failwith "The given pos doesn't exist in this queue"
  | (pos_q, d_q) :: tl ->
      if (x, y) = pos_q then (pos_q, new_value) :: tl
      else (pos_q, d_q) :: dicrease_pos_queue tl (x, y) new_value

let create_queue_dijkstra g d =
  List.init g.size (fun i ->
      let (x, y), _ = g.adj.(i) in
      ((x, y), d.(y).(x)))

let init_distance pos =
  let x, y = pos in
  let a =
    Array.make_matrix board_size board_size (board_size * board_size)
    (* A path cannot be longer than the board area *)
  in
  a.(y).(x) <- 0;
  a

let init_pre_pos () : position option array array =
  Array.make_matrix board_size board_size None

let init_dijkstra player_pos =
  let g = init_graph player_pos in
  let dst = init_distance player_pos in
  let q = create_queue_dijkstra g dst in
  let pre_pos = init_pre_pos () in
  (g, dst, q, pre_pos)

let distance_dijkstra player_pos =
  let g, dst, q, pre_pos = init_dijkstra player_pos in
  let rec aux_distance_dijkstra q_acc =
    if q_acc = [] then (dst, pre_pos)
    else
      let x_min, y_min = get_min_distance_of_queue q_acc in
      let d_min = dst.(y_min).(x_min) in
      let q_acc = remove_queue q_acc (x_min, y_min) in
      let adj_m = get_adjacent_positions g (x_min, y_min) in
      let rec dicrease_dijstra adj_acc q_dicrease_acc =
        match adj_acc with
        | [] -> q_dicrease_acc
        | (x, y) :: tl ->
            if dst.(y).(x) > d_min + 1 then (
              dst.(y).(x) <- 1 + d_min;
              pre_pos.(y).(x) <- Some (x_min, y_min);
              let q_dicrease_acc =
                dicrease_pos_queue q_dicrease_acc (x, y) dst.(y).(x)
              in
              dicrease_dijstra tl q_dicrease_acc)
            else dicrease_dijstra tl q_dicrease_acc
      in
      let q_acc = dicrease_dijstra adj_m q_acc in
      aux_distance_dijkstra q_acc
  in
  aux_distance_dijkstra q

let find_nearest_winning_position dst pos_init =
  let x_init, y_init = pos_init in
  let win_position = get_winneable_positions (x_init, y_init) in
  let rec get_min_distance_winneable_position pos_acc min_d_acc min_pos_acc =
    match pos_acc with
    | [] -> min_pos_acc
    | (x, y) :: tl ->
        let d = dst.(y).(x) in
        if d < min_d_acc then get_min_distance_winneable_position tl d (x, y)
        else get_min_distance_winneable_position tl min_d_acc min_pos_acc
  in
  get_min_distance_winneable_position win_position max_int (0, 0)

let find_next_move_for_winning_position player_pos pre pos_win =
  let x_win, y_win = pos_win in
  let rec aux_get_next_move_with_with_position (x, y) =
    match pre.(y).(x) with
    | None -> failwith "Unknown pre pos of winneable position"
    | Some (x_pre, y_pre) ->
        if (x_pre, y_pre) = player_pos then (x, y)
        else aux_get_next_move_with_with_position (x_pre, y_pre)
  in
  aux_get_next_move_with_with_position (x_win, y_win)

let choose_move_gabin_bot player_pos =
  let dst, pre_pos = distance_dijkstra player_pos in
  let x, y =
    find_nearest_winning_position dst (current_player ()).start_position
  in
  find_next_move_for_winning_position player_pos pre_pos (x, y)

let will_walls_block_player wall_pos1 wall_pos2 =
  let all_player_current_pos =
    List.map (fun (_, current_pos) -> current_pos) (get_all_player_pos ())
  in
  List.exists
    (fun current_pos ->
      List.for_all
        (fun pos_vect ->
          let x = fst current_pos + fst pos_vect in
          let y = snd current_pos + snd pos_vect in
          (x, y) = wall_pos1
          || (x, y) = wall_pos2
          ||
          try
            is_wall
              (fst current_pos + fst pos_vect, snd current_pos + snd pos_vect)
          with InvalidPosition _ -> false)
        move_vectors)
    all_player_current_pos

let find_free_place_wall_around pos_wall =
  let x_wall, y_wall = pos_wall in
  let vec = move_vectors in
  let rec try_to_get_valide_position_around_wall v_list =
    match v_list with
    | [] -> None
    | (v1, v2) :: tl ->
        let new_x = x_wall + v1 in
        let new_y = y_wall + v2 in
        if
          (try
             validate_position (new_x, new_y);
             true
           with _ -> false)
          && is_wall_position (new_x, new_y)
          && not (is_wall (new_x, new_y))
        then Some (new_x, new_y)
        else try_to_get_valide_position_around_wall tl
  in
  try_to_get_valide_position_around_wall vec

(* Gives a random wall if it exists and has been chosen by random and returns None otherwise *)
let get_random_wall_position_around_player pos_player =
  let x_player, y_player = pos_player in
  let lst_moves = list_of_moves (x_player, y_player) in
  let lst_moves =
    List.filter
      (fun pos2 ->
        try not (is_wall_between (x_player, y_player) pos2) with _ -> false)
      lst_moves
  in
  let lst_moves_length = List.length lst_moves in
  let rec try_to_get_around_wall_positions lst_moves_acc =
    match lst_moves_acc with
    | [] -> None
    | (x, y) :: tl -> (
        let r = Random.int lst_moves_length in
        if r <> 0 then try_to_get_around_wall_positions tl
        else
          let wall_position1 = ((x_player + x) / 2, (y_player + y) / 2) in
          let wall_position2_opt = find_free_place_wall_around wall_position1 in
          match wall_position2_opt with
          | None -> try_to_get_around_wall_positions tl
          | Some wall_position2 -> (
              try
                validate_wall_placement (current_player ()).walls_left
                  wall_position1 wall_position2;
                if will_walls_block_player wall_position1 wall_position2 then
                  None
                else Some (wall_position1, wall_position2)
              with _ -> None))
  in
  try_to_get_around_wall_positions lst_moves

let choose_wall_gabin_bot other_player_positions =
  let r = Random.int (List.length other_player_positions) in
  let pos_player = List.nth other_player_positions r in
  get_random_wall_position_around_player pos_player

let det_move_gabin_bot player_pos =
  let other_player_positions =
    get_all_player_positions_except (current_player ()).current_position
  in
  if
    (current_player ()).walls_left > 0
    &&
    let r = Random.int 3 in
    r = 0
  then
    match choose_wall_gabin_bot other_player_positions with
    | None -> Moving (choose_move_gabin_bot player_pos)
    | Some (wall_position1, wall_position2) ->
        Placing_wall (wall_position1, wall_position2)
  else Moving (choose_move_gabin_bot player_pos)
