open Board
open Types

(** Replace the element of indice i with a new element
    @param i the index of the replaced element, list the list of element, element the new element
    @return the list with updated element of indice i
    *)
let list_replace_elem (i' : int) (list : 'a list) (elem : 'a) : 'a list =
  List.mapi (fun i p -> if i = i' then elem else p) list

(** Returns the absolute value of the difference between opposite and the y value of position (vertical)
    @param curr the current position of a player, opposite the goal of the player 
    @return absolute value of the difference between opposite and the y value of position
    *)
let dist_from_opposite_vertical (curr : position) (opposite : int) : int =
  abs (opposite - snd curr)

(** Returns the absolute value of the difference between opposite and the x value of position (horizontal)
    @param curr the current position of a player, opposite the goal of the player 
    @return absolute value of the difference between opposite and the x value of position
    *)
let dist_from_opposite_horizontal (curr : position) (opposite : int) : int =
  abs (opposite - fst curr)

(** Add the information of a cell to the queue, and proceed to sort the queue in order to implement a priority. The cells are sorted from to closest to the goal to the farthest.
    @param queue the current state of the queue, cell is the information to add to the queue
    @return the updated state of the queue
    *)
let add_to_queue (queue : (int * int * position) list)
    (cell : int * int * position) : (int * int * position) list =
  let l = cell :: queue in
  List.sort
    (fun v1 v2 ->
      let f1, h1, _ = v1 in
      let f2, h2, _ = v2 in
      if f1 > f2 then 1
      else if f1 < f2 then -1
      else if h1 > h2 then 1
      else if h1 < h2 then -1
      else 0)
    l

(** Returns the element with the highest priority in the queue and removes it
    @param queue the current state of the queue
    @return None if the queue is empty, the element with the highest priority and the updated queue
    *)
let pull_from_queue (queue : (int * int * position) list) :
    (position * (int * int * position) list) option =
  match queue with
  | [] -> None
  | h :: t ->
      let _, _, p = h in
      Some (p, t)

(** Returns the list of position to get to the goal, the list represent the optimal path to get the opposite of the board
    @param init_pos the starting position of the player, color the color of the player looking for the path
    @return the list of positions that represent the shortest path to the goal
    *)
let prediction (init_pos : position) (color : color) : position list =
  let opposite =
    match color with Red | Green -> board_size - 1 | Blue | Yellow -> 0
  in
  let dist_from_opposite =
    match color with
    | Red | Blue -> dist_from_opposite_vertical
    | Green | Yellow -> dist_from_opposite_horizontal
  in
  (* Init *)
  let queue = [] in
  let g_score_list =
    List.init board_size (fun _ ->
        List.init board_size (fun _ -> board_size * board_size))
  in
  let g_score_list =
    list_replace_elem (fst init_pos) (List.nth g_score_list (snd init_pos)) 0
    |> list_replace_elem (snd init_pos) g_score_list
  in
  let f_score_list =
    List.init board_size (fun _ ->
        List.init board_size (fun _ -> board_size * board_size))
  in
  let f_score_list =
    list_replace_elem (fst init_pos)
      (List.nth f_score_list (snd init_pos))
      (dist_from_opposite init_pos opposite)
    |> list_replace_elem (snd init_pos) f_score_list
  in
  let queue =
    add_to_queue queue
      ( dist_from_opposite init_pos opposite,
        dist_from_opposite init_pos opposite,
        init_pos )
  in
  let path =
    List.init board_size (fun _ -> List.init board_size (fun _ -> init_pos))
  in
  (* A* algorithm *)
  let rec pathfinding queue path g_list f_list =
    let res = pull_from_queue queue in
    if res = None then (None, path)
    else
      let curr, queue = Option.get res in
      if dist_from_opposite curr opposite = 0 then (Some curr, path)
      else
        let possible_moves = list_of_moves curr in
        let rec aux moves queue path g_list f_list =
          match moves with
          | [] -> (queue, path, g_list, f_list)
          | h :: t ->
              let tmp_g =
                List.nth (List.nth g_list (snd curr)) (fst curr) + 1
              in
              let tmp_f = tmp_g + dist_from_opposite h opposite in
              if tmp_f < List.nth (List.nth g_list (snd h)) (fst h) then
                let g_list' =
                  list_replace_elem (fst h) (List.nth g_list (snd h)) tmp_g
                  |> list_replace_elem (snd h) g_list
                in
                let f_list' =
                  list_replace_elem (snd h) (List.nth f_list (fst h)) tmp_f
                  |> list_replace_elem (fst h) f_list
                in
                let queue' =
                  add_to_queue queue (tmp_f, dist_from_opposite h opposite, h)
                in
                let path' =
                  list_replace_elem (snd h) (List.nth path (fst h)) curr
                  |> list_replace_elem (fst h) path
                in
                aux t queue' path' g_list' f_list'
              else aux t queue path g_list f_list
        in
        let queue', path', g_list', f_list' =
          aux possible_moves queue path g_list f_list
        in
        pathfinding queue' path' g_list' f_list'
  in
  let finish, path' = pathfinding queue path g_score_list f_score_list in
  if finish = None then
    raise (NoMovePossible "There is no movement possible for this player")
  else
    let finish_pos = Option.get finish in
    let rec rewind pos trace =
      if fst pos = fst init_pos && snd pos = snd init_pos then trace
      else rewind (List.nth (List.nth path' (fst pos)) (snd pos)) (pos :: trace)
    in
    let rewinded_finish = rewind finish_pos [] in
    rewinded_finish

(** Returns the first position of the list as a Move
    @param list the path to the goal
    @return the first element of the list
    *)
let move_from_list list = Moving (List.nth list 0)

(** Returns the positions of the wall
    @param pos_enemy the current position of the enemy
    @raise InvalidPosition (pos, "Position is outside the board boundaries")) in some cases
    @return the positions of the wall
    *)
let find_good_wall pos_enemy =
  let enemy_color =
    match (current_player ()).color with
    | Red -> Blue
    | Blue -> Red
    | Yellow -> Green
    | Green -> Yellow
  in
  let comp, incr =
    match enemy_color with
    | Red | Green -> (( < ), ( + ))
    | Blue | Yellow -> (( > ), ( - ))
  in
  let path_enemy = prediction pos_enemy enemy_color in
  let next_enemy_pos = List.nth path_enemy 0 in

  (* FORWARD *)
  if comp (snd pos_enemy) (snd next_enemy_pos) then
    if
      (not (is_wall (fst pos_enemy, incr (snd pos_enemy) 1)))
      && not (is_wall (fst pos_enemy + 1, incr (snd pos_enemy) 1))
    then
      Some
        ( (fst pos_enemy, incr (snd pos_enemy) 1),
          (fst pos_enemy + 1, incr (snd pos_enemy) 1) )
    else if
      (not (is_wall (fst pos_enemy, incr (snd pos_enemy) 1)))
      && not (is_wall (fst pos_enemy - 1, incr (snd pos_enemy) 1))
    then
      Some
        ( (fst pos_enemy, incr (snd pos_enemy) 1),
          (fst pos_enemy - 1, incr (snd pos_enemy) 1) )
    else None (* RIGHT *)
  else if fst pos_enemy < fst next_enemy_pos then
    if
      (not (is_wall (fst pos_enemy + 1, snd pos_enemy)))
      && not (is_wall (fst pos_enemy + 1, snd pos_enemy + 1))
    then
      Some
        ( (fst pos_enemy + 1, snd pos_enemy),
          (fst pos_enemy + 1, snd pos_enemy + 1) )
    else if
      (not (is_wall (fst pos_enemy + 1, snd pos_enemy)))
      && not (is_wall (fst pos_enemy + 1, snd pos_enemy - 1))
    then
      Some
        ( (fst pos_enemy + 1, snd pos_enemy),
          (fst pos_enemy + 1, snd pos_enemy - 1) )
    else None (* LEFT *)
  else if fst pos_enemy > fst next_enemy_pos then
    if
      (not (is_wall (fst pos_enemy - 1, snd pos_enemy)))
      && not (is_wall (fst pos_enemy - 1, snd pos_enemy + 1))
    then
      Some
        ( (fst pos_enemy - 1, snd pos_enemy),
          (fst pos_enemy - 1, snd pos_enemy + 1) )
    else if
      (not (is_wall (fst pos_enemy - 1, snd pos_enemy)))
      && not (is_wall (fst pos_enemy - 1, snd pos_enemy - 1))
    then
      Some
        ( (fst pos_enemy - 1, snd pos_enemy),
          (fst pos_enemy - 1, snd pos_enemy - 1) )
    else None
  else None

(** Choses when to place a wall or to move
    @param pos the current position of the current player
    @return the new move
    *)
let optimized_move_with_walls (pos : position) =
  let opposite =
    match (current_player ()).color with
    | Red | Green -> board_size - 1
    | _ -> 0
  in
  let enemy =
    snd
      (List.nth
         (get_all_player_pos ()
         |> List.filter (fun (_, current) ->
                fst current <> fst pos || snd current <> snd pos))
         0)
  in
  if
    dist_from_opposite_vertical enemy opposite = 6
    && (current_player ()).walls_left > 0
  then
    let wall = find_good_wall enemy in
    if wall <> None then
      let a, b = Option.get wall in
      Placing_wall (a, b)
    else move_from_list (prediction pos (current_player ()).color)
  else move_from_list (prediction pos (current_player ()).color)
