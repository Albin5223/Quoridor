open Board
open Types

let target_zone player =
  match player.start_position with
  | x, _ when x = 0 ->
      (* Start at left border *)
      fun (x, _) -> x = board_size - 1
  | x, _ when x = board_size - 1 ->
      (* Start at right border *)
      fun (x, _) -> x = 0
  | _, y when y = 0 ->
      (* Start at top border *)
      fun (_, y) -> y = board_size - 1
  | _, y when y = board_size - 1 ->
      (* Start at bottom border *)
      fun (_, y) -> y = 0
  | _ ->
      raise
        (InvalidPlayerPosition (player.start_position, "Invalid start position"))

let build_tab (pos : position) =
  let p = current_player () in
  let visited = Array.make_matrix board_size board_size 100 in

  let rec dfs lastval pos =
    let x, y = pos in
    let newval = lastval + 1 in
    visited.(y).(x) <- newval;
    let is_target_pos = target_zone p in
    if not (is_target_pos pos) then
      let next_moves = list_of_moves pos in
      let filtered_next_moves =
        List.filter (fun (x, y) -> visited.(y).(x) > newval + 1) next_moves
      in
      List.iter (fun po -> dfs newval po) filtered_next_moves
  in
  dfs (-1) pos;
  visited

let terminal_positions player =
  if target_zone player (1, board_size - 1) then fun x -> (x, board_size - 1)
  else if target_zone player (1, 0) then fun x -> (x, 0)
  else if target_zone player (board_size - 1, 1) then fun x ->
    (board_size - 1, x)
  else fun x -> (0, x)

let is_valid_pos x y = x >= 0 && x < board_size && y >= 0 && y < board_size

let find lst tab pos =
  let rec my_find lst tab (x, y) dist =
    match lst with
    | [] -> raise Not_found
    | (a, b) :: xs ->
        if is_valid_pos (x + (2 * a)) (y + (2 * b)) then
          if (fun (a, b) -> tab.(y + (2 * b)).(x + (2 * a)) = dist - 1) (a, b)
          then (x + (2 * a), y + (2 * b))
          else if tab.(y + (2 * b)).(x + (2 * a)) = 100 then
            try
              let i, j =
                my_find move_vectors tab (x + (2 * a), y + (2 * b)) dist
              in
              (i, j)
            with Not_found -> my_find xs tab (x, y) dist
          else my_find xs tab (x, y) dist
        else my_find xs tab (x, y) dist
  in
  my_find lst tab pos

let find_shorter_path tab (i, j) : position list =
  let p = current_player () in
  let targets = List.init board_size (fun a -> terminal_positions p a) in
  let rec aux path (x, y) =
    if (x, y) = (i, j) then path
    else
      let vx, vy = find move_vectors tab (x, y) tab.(y).(x) in
      aux ((vx, vy) :: path) (vx, vy)
  in
  let rec smaller l (x, y) =
    match l with
    | [] -> (x, y)
    | (x', y') :: s ->
        if (not (is_valid_pos x y)) || tab.(y').(x') < tab.(y).(x) then
          smaller s (x', y')
        else smaller s (x, y)
  in
  aux
    [ smaller targets (board_size, board_size) ]
    (smaller targets (board_size, board_size))

let strat_ia pos =
  let tab = build_tab pos in

  let path = find_shorter_path tab pos in

  let a, b = List.nth path 1 in

  Moving (a, b)
