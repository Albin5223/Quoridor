open Types
open Board

(** 
  What is wall_detection_radius ?
  We want to avoid path where there is a lot of walls around us. So we count the number of walls around the path, and we take the path where there is the less walls around it.
  But the bot will not prioritize this concept. If there is a path with a lot of walls, but it is shorter than another with less walls, the bot will take the shortest path.
  This detection is only happenning when the bot has more that two paths to choose from.
  
  2 is a good compromise 
*)
let wall_detection_radius = 2

(**
  What is near_wall_scanner ?
  After finding the best wall to place, we only place it when the player is near the emplacement of the wall.
  This variable is the radius of the circle around the emplacement of the wall.
  If the player is in the circle, we place the wall.
  If the player is not in the circle, we don't place the wall.
  
  Why do we need this ? If we place the wall right away while the opponent is far away, he will decide himself to go somewhere else to early.
  So we wait until he is near the emplacement of the wall, and we place it, so he loses time.
*)
let near_wall_scanner = 3

(**
   Structure of a priority queue
   Usefull to implement the BFS algorithm, and find the shortest path
    @param priority the priority of the element
*)
module PriorityQueue = struct
  let create () = ref []
  let is_empty q = !q = []

  let rec insert q priority elt =
    match !q with
    | [] -> q := [ (priority, elt) ]
    | (p, e) :: rest ->
        if priority < p then q := (priority, elt) :: !q
        else
          let rest_q = ref rest in
          insert rest_q priority elt;
          q := (p, e) :: !rest_q

  let pop q =
    match !q with
    | [] -> failwith "Cannot pop from an empty queue"
    | (p, e) :: rest ->
        q := rest;
        (p, e)
end

(** Type of a side *)
type side = Top | Bottom | Left | Right

(** Type of a square *)
type square = Empty of position | Wall of position | Player of position

type graph = { sommets : position list; aretes : (position * position) list }
(** Type of a graph *)

(* <<<<<<<<<< All the prints >>>>>>>>>> *)

let print_position position =
  let x, y = position in
  print_string "(";
  print_int x;
  print_string ", ";
  print_int y;
  print_string ")"

(* <<<<<<<<<< End of prints >>>>>>>>>> *)

(**
    This function gives the union of two lists
    @param list1 the first list
    @param list2 the second list
    @return the union of the two lists
*)
let union list1 list2 =
  let combined = List.append list1 list2 in
  List.sort_uniq compare combined

(**
    This function gives the opposite side of a side
    @param side the side
    @return the opposite side
*)
let get_opposite_side (side : side) : side =
  match side with
  | Top -> Bottom
  | Bottom -> Top
  | Left -> Right
  | Right -> Left

(**
    This function gives the side of a position, example : (_, 0) -> Top, (0, _) -> Left, etc.
    @param start_pos the position
    @return the side of the position
*)
let get_side (start_pos : position) : side =
  let x, y = start_pos in
  if x = 0 then Left
  else if x = board_size - 1 then Right
  else if y = 0 then Top
  else if y = board_size - 1 then Bottom
  else failwith "not a side"

(**
    This function let us know if a position is in the board
    @param pos the position
    @return true if the position is in the board, false otherwise    
*)
let is_in_board (pos : position) : bool =
  let x, y = pos in
  x >= 0 && x < board_size && y >= 0 && y < board_size

(**
    This function gives us the starting position of a player    
    @param pos the position of a player
    @return the starting position of a player
*)
let get_start_pos (pos : position) : position =
  let all_player = get_all_player_pos () in
  let our_player = List.filter (fun (_, c) -> c = pos) all_player in
  if List.length our_player = 0 then
    failwith "position is not a player's position"
  else List.hd our_player |> fst

(**
    This function gives all of the positions of a side.
    @param side the side
    @return the list of positions of the side
*)
let get_position_list_of_side (side : side) : position list =
  match side with
  | Top ->
      List.init board_size (fun x -> x)
      |> List.filter (fun x -> x mod 2 = 0)
      |> List.map (fun x -> (x, 0))
  | Bottom ->
      List.init board_size (fun x -> x)
      |> List.filter (fun x -> x mod 2 = 0)
      |> List.map (fun x -> (x, board_size - 1))
  | Left ->
      List.init board_size (fun y -> y)
      |> List.filter (fun y -> y mod 2 = 0)
      |> List.map (fun y -> (0, y))
  | Right ->
      List.init board_size (fun y -> y)
      |> List.filter (fun y -> y mod 2 = 0)
      |> List.map (fun y -> (board_size - 1, y))

(**
    This function gives the jump edges, considering the position of the player and the position of the opponent  
    @param my_pos the position of the player
    @param his_pos the position of the opponent 
    @return the list of edges
*)
let get_jump_aretes_from_someone (my_pos : position) (his_pos : position) :
    (position * position) list =
  (* Function to get the middle position between two positions *)
  let middle_position ((x1, y1) : position) ((x2, y2) : position) : position =
    ((x1 + x2) / 2, (y1 + y2) / 2)
  in
  (* Function to get the opposite position of a position from "his_pos" *)
  let opposite_position (p : position) : position =
    let dx = fst p - fst his_pos in
    let dy = snd p - snd his_pos in
    (fst his_pos - dx, snd his_pos - dy)
  in
  (* Function to get the two diagonal positions of a position from "his_pos" *)
  let diagonal_positions (p : position) : position * position =
    let dx = fst p - fst his_pos in
    let dy = snd p - snd his_pos in
    ((fst his_pos - dy, snd his_pos + dx), (fst his_pos + dy, snd his_pos - dx))
  in
  let his_x, his_y = his_pos in
  let adjacent_positions =
    [
      (his_x - 2, his_y);
      (his_x + 2, his_y);
      (his_x, his_y - 2);
      (his_x, his_y + 2);
    ]
  in
  let filtered_adj_pos =
    (* We cannot jump to pos if pos is outside the board OR if a wall is between the player and the pos OR if a player is already in the pos (except us) *)
    List.filter
      (fun pos ->
        is_in_board pos
        && (not (is_wall (middle_position his_pos pos)))
        && not
             (List.mem pos
                (List.map (fun (_, cur) -> cur) (get_all_player_pos ()))
             && pos <> my_pos))
      adjacent_positions
  in
  (* We get all the edges *)
  let all_aretes =
    List.map
      (fun p ->
        (* If the opposite position of p is here too, we can only jump to there from p (and not the diagonals) *)
        if List.mem (opposite_position p) filtered_adj_pos then
          [ (p, opposite_position p) ]
          (* Else, we check if we can jump in diagonal to left and right*)
        else
          let list =
            let left = fst (diagonal_positions p) in
            (* If we can jump to the left diagonal, we add the edge *)
            if List.mem left filtered_adj_pos then [ (p, left) ] else []
          in
          let list2 =
            let right = snd (diagonal_positions p) in
            (* If we can jump to the right diagonal, we add the edge *)
            if List.mem right filtered_adj_pos then list @ [ (p, right) ]
            else list
          in
          list2)
      filtered_adj_pos
  in
  (* Finally, we flatten to get all of the egdes *)
  List.flatten all_aretes

(**
    This function gives a copy of the board
    @return a copy of the board    
*)
let copy_board () : square list list =
  let all_walls = get_all_wall_pos () in
  let all_players = List.map (fun (_, c) -> c) (get_all_player_pos ()) in
  let rec loop_row board y =
    (* loop through rows *)
    match y with
    | size_y when size_y = board_size ->
        List.rev board
        (* if we reached the end of the board, return the reversed board *)
    | _ ->
        let rec loop_col rows x =
          (* loop through columns *)
          match x with
          | size_x when size_x = board_size ->
              List.rev rows
              (* if we reached the end of the row, return the reversed row *)
          | _ ->
              if List.mem (x, y) all_walls then
                (* if the current position is a wall, add a wall to the row *)
                loop_col (Wall (x, y) :: rows) (x + 1)
              else if List.mem (x, y) all_players then
                (* if the current position is a player, add a player to the row *)
                loop_col (Player (x, y) :: rows) (x + 1)
              else
                (* if the current position is empty, add an empty square to the row *)
                loop_col (Empty (x, y) :: rows) (x + 1)
        in
        loop_row (loop_col [] 0 :: board) (y + 1)
    (* add the row to the board *)
  in
  loop_row [] 0 (* start the loop with an empty board and y = 0 *)

(** 
    This function gives the current graph of the board
    @param current_position the current position of the player
    @return the graph of the board
*)
let get_graph (current_position : position) : graph =
  (* We get the board with a copy *)
  let board = copy_board () in
  (* We flatten the board *)
  let flatten_board = List.flatten board in
  (* We get all the walls *)
  let all_walls = get_all_wall_pos () in
  (* We get all the players *)
  let all_players = List.map (fun (_, c) -> c) (get_all_player_pos ()) in
  (* We get all the empty nodes & the players node *)
  let all_sommets =
    all_players
    @ List.map
        (fun x -> match x with Empty pos -> pos | _ -> failwith "pas normal")
        (List.filter
           (fun x ->
             match x with
             | Empty (x, y) -> x mod 2 = 0 && y mod 2 = 0
             | _ -> false)
           flatten_board)
  in
  (* We get all the edges *)
  let all_aretes =
    let rec loop_sommets aretes sommets =
      match sommets with
      | [] -> aretes
      | (x, y) :: next ->
          (* We get the adjacent nodes & wall, the first pos is the node where we can place a piece, the second one is for the wall between the bot and the node *)
          let node_and_wall_adjacents =
            [
              ((x - 2, y), (x - 1, y));
              (* left *)
              ((x + 2, y), (x + 1, y));
              (* right *)
              ((x, y + 2), (x, y + 1));
              (* bottom *)
              ((x, y - 2), (x, y - 1));
              (* top *)
            ]
          in
          let valid_adjacents =
            List.filter
              (fun pos ->
                List.mem (fst pos) all_sommets
                && (* check if the adjacent node is a node *)
                not (List.mem (snd pos) all_walls))
                (* check if there is a wall between the node and the adjacent node *)
              node_and_wall_adjacents
          in
          let new_aretes =
            List.map (fun (pos, _) -> ((x, y), pos)) valid_adjacents
          in
          (* create the aretes *)
          loop_sommets (new_aretes @ aretes) next
    in
    loop_sommets [] all_sommets
  in
  (* We take the list of players excluding me *)
  let all_players_without_me =
    List.filter (fun x -> x <> current_position) all_players
  in
  (* We get the jumping edges for the player *)
  let all_aretes_with_jump =
    List.flatten
      (List.map
         (get_jump_aretes_from_someone current_position)
         all_players_without_me)
  in
  { sommets = all_sommets; aretes = union all_aretes all_aretes_with_jump }

(**
    This function gives all of the positions that are between two positions
    @param p1 the first position
    @param p2 the second position
    @return the list of positions between p1 and p2    
*)
let all_positions_between (p1 : position) (p2 : position) : position list =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  let min_x, max_x = (min x1 x2, max x1 x2) in
  let min_y, max_y = (min y1 y2, max y1 y2) in
  let rec loop x y acc =
    if x > max_x then acc
    else if y > max_y then loop (x + 1) min_y acc
    else loop x (y + 1) ((x, y) :: acc)
  in
  loop min_x min_y []

(**
    This function gives the new graph when we add a wall.
    We remove the edges that are on the wall & the edges where the wall is between the extremities
    @param g the graph
    @param wall the wall to add
    @return the new graph    
*)
let add_wall_to_graph (g : graph) (wall : position * position) : graph =
  let new_aretes =
    List.filter
      (fun (u, v) ->
        u <> fst wall
        && u <> snd wall
        && v <> fst wall
        && v <> snd wall
        &&
        let all_positions_between = all_positions_between u v in
        (not (List.mem (fst wall) all_positions_between))
        && not (List.mem (snd wall) all_positions_between))
      g.aretes
  in
  { sommets = g.sommets; aretes = new_aretes }

(** 
    This function gives the shortest path from start to end_ using the BFS algorithm
    @param g the graph
    @param start the starting position
    @param end_ the ending position
    @return the shortest path from start to end_ if it exists, None otherwise
*)
let bfs (g : graph) (start : position) (end_ : position) : position list =
  (* We give a priority to a position, the priority is the distance between the position and the end_ *)
  let priority (pos : position) : int =
    let dx = fst end_ - fst pos in
    let dy = snd end_ - snd pos in
    int_of_float (sqrt ((float_of_int dx ** 2.) +. (float_of_int dy ** 2.)))
  in
  if (not (List.mem start g.sommets)) || not (List.mem end_ g.sommets) then []
  else
    let queue = PriorityQueue.create () in
    let pred = Hashtbl.create (List.length g.sommets) in
    PriorityQueue.insert queue (priority start) start;
    let rec loop () =
      if PriorityQueue.is_empty queue then []
      else
        let _, v = PriorityQueue.pop queue in
        if v = end_ then
          let rec path v acc =
            if v = start then acc else path (Hashtbl.find pred v) (v :: acc)
          in
          path end_ []
        else
          let adjacents =
            List.map snd (List.filter (fun (x, _) -> x = v) g.aretes)
          in
          List.iter
            (fun w ->
              if not (Hashtbl.mem pred w) then (
                PriorityQueue.insert queue (priority w) w;
                Hashtbl.add pred w v))
            adjacents;
          loop ()
    in
    loop ()

(**
    Function to count the number of walls around a position (in a radius)
    @param pos the position
    @param radius the radius
    @return the number of walls around the position    
*)
let count_walls_around (pos : position) (radius : int) : int =
  let directions =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let positions_around =
    List.concat
      (List.map
         (fun (dx, dy) ->
           List.init radius (fun i ->
               let j = i + 1 in
               (fst pos + (j * dx), snd pos + (j * dy))))
         directions)
  in
  List.fold_left
    (fun acc pos ->
      acc + if not (is_in_board pos) then 0 else if is_wall pos then 1 else 0)
    0 positions_around

(**
    This function gives the move to do to reach the arrival using bfs ("pos" in not included in the path)
    @param g the graph
    @param pos the current position of the player
    @return the move to do to reach the arrival    
*)
let get_best_path_with_bfs (g : graph) (pos : position)
    (wall_detection_radius : int) : position list =
  (* We want to prioritize the path where there is the less walls around it *)
  let priority_between_two_path (path1 : position list) (path2 : position list)
      =
    (* The path1 is shorter *)
    if List.length path1 < List.length path2 then path1
      (* The path2 is shorter *)
    else if List.length path1 > List.length path2 then path2
      (* The path1 and path2 have the same length, but we wants to take the path where there is the less walls at sides *)
    else
      let count_walls_around_paths (path1 : position list)
          (path2 : position list) (radius : int) : position list =
        let count_walls (count1, count2) (pos1, pos2) =
          ( count1 + count_walls_around pos1 radius,
            count2 + count_walls_around pos2 radius )
        in
        let wall_counts =
          List.fold_left count_walls (0, 0) (List.combine path1 path2)
        in
        if fst wall_counts < snd wall_counts then path1 else path2
      in
      count_walls_around_paths path1 path2 wall_detection_radius
  in
  let start = pos in
  let rec choose_path (arrival : position list) (old_path : position list) =
    match arrival with
    | [] -> old_path
    | h :: t -> (
        let new_path = bfs g start h in
        match (old_path, new_path) with
        | [], [] -> choose_path t []
        | [], _ -> choose_path t new_path
        | _, [] -> choose_path t old_path
        | old, new_ -> choose_path t (priority_between_two_path old new_))
  in
  let arrival_side = get_opposite_side (get_side (get_start_pos pos)) in
  let arrival_list = get_position_list_of_side arrival_side in
  choose_path arrival_list []

(**
    Function that gives the opponent's position with the shortest path
    @param pos the current position of the player (my bot)
    @return the opponent's position with the shortest path    
*)
let closest_opponent (pos : position) : position =
  let opponent_list =
    List.filter (fun (_, c) -> c <> pos) (get_all_player_pos ())
  in
  let opponent_pos_and_path =
    List.map
      (fun (_, cur) ->
        (cur, get_best_path_with_bfs (get_graph cur) cur wall_detection_radius))
      opponent_list
  in
  let compare_pos_and_path (pos1, path1) (pos2, path2) =
    if List.length path1 < List.length path2 then (pos1, path1)
    else (pos2, path2)
  in
  let opponent_pos, _ =
    List.fold_left compare_pos_and_path
      (List.hd opponent_pos_and_path)
      (List.tl opponent_pos_and_path)
  in
  opponent_pos

(**
    Function to see where is the best position to place a wall (increase the path of the opponent the most)
    @param g the opponent's graph
    @param opponent_pos the position of the opponent
    @param walls the list of walls that we can place    
*)
let best_wall_to_place (g : graph) (opponent_pos : position)
    (walls : (position * position) list) : (position * position) option =
  let path_length_with_wall (wall : position * position) : int =
    let g_with_wall = add_wall_to_graph g wall in
    let path_with_wall =
      get_best_path_with_bfs g_with_wall opponent_pos wall_detection_radius
    in
    List.length path_with_wall
  in
  let wall_lengths =
    List.map (fun wall -> (wall, path_length_with_wall wall)) walls
  in
  let sorted_walls =
    List.sort (fun (_, len1) (_, len2) -> compare len2 len1) wall_lengths
  in
  match sorted_walls with (wall, _) :: _ -> Some wall | [] -> None

(**
    This function gives the wall to place to block the opponent (choose the opponent with the shortest path)
    @param pos the current position of the player (my bot)
    @param near_radius the radius of the circle around the emplacement of the wall
    @return the wall to place (if there is no wall to place, return None)
*)
let place_wall (pos : position) (near_radius : int) :
    (position * position) option =
  let me = current_player () in
  (* If the player doesn't have walls, we return None *)
  if me.walls_left = 0 then None
  else
    let wall_left = me.walls_left in
    let opponent_pos = closest_opponent pos in
    let opponent_path =
      get_best_path_with_bfs (get_graph opponent_pos) opponent_pos
        wall_detection_radius
    in
    (* All of the orientation of a wall from a position *)
    let walls_possible (p : position) : (position * position) list =
      [
        (p, (fst p - 1, snd p));
        (p, (fst p + 1, snd p));
        (p, (fst p, snd p + 1));
        (p, (fst p, snd p - 1));
      ]
    in
    let middle_position ((x1, y1) : position) ((x2, y2) : position) : position =
      ((x1 + x2) / 2, (y1 + y2) / 2)
    in
    let rec pos_between_steps (op_path : position list) acc =
      match op_path with
      (* should never happen, because the opponent has not won yet *)
      | [] -> []
      (* end of the path *)
      | _ :: [] -> acc
      (* we store the pos between the two pos *)
      | p1 :: p2 :: t ->
          pos_between_steps (p2 :: t) (acc @ [ middle_position p1 p2 ])
    in
    (* Getting the list of walls that I can place between each two steps in a list *)
    let possible_walls_per_step =
      List.map
        (fun (x, y) ->
          List.filter
            (fun (wall_1, wall_2) ->
              try
                validate_wall_placement wall_left wall_1 wall_2;
                true
              with _ -> false)
            (walls_possible (x, y)))
        (pos_between_steps ([ opponent_pos ] @ opponent_path) [])
    in
    (* List of all the walls that we can place to increase his path *)
    let possible_walls = List.flatten possible_walls_per_step in
    if List.length possible_walls = 0 then None
    else
      (* But we want to place a wall that doesn't increase our path, so this function verify if a wall is between two positions *)
      let is_wall_between (pos1 : position) (pos2 : position)
          ((wall1, wall2) : position * position) : bool =
        let all_positions_between = all_positions_between pos1 pos2 in
        List.mem wall1 all_positions_between
        || List.mem wall2 all_positions_between
      in
      (* Function to see if there is a wall between two steps of a path *)
      let is_wall_in_path (wall : position * position) (path : position list) :
          bool =
        let rec aux path =
          match path with
          | pos1 :: pos2 :: rest ->
              if is_wall_between pos1 pos2 wall then true else aux (pos2 :: rest)
          | _ -> false
        in
        aux path
      in
      (* We include our position to the path *)
      let my_path =
        [ pos ]
        @ get_best_path_with_bfs (get_graph pos) pos wall_detection_radius
      in
      (* We filter the walls that are in our path *)
      let filter_wall =
        List.filter
          (fun wall -> not (is_wall_in_path wall my_path))
          possible_walls
      in
      (* If there is no wall that don't bother us, we move *)
      if List.length filter_wall = 0 then None
      else
        (* We take the best wall to place *)
        let best_wall =
          best_wall_to_place (get_graph opponent_pos) opponent_pos filter_wall
        in
        if best_wall = None then None
        else
          let is_near_wall (pos : position)
              ((wall1, wall2) : position * position) (radius : int) : bool =
            let distance (pos1 : position) (pos2 : position) : float =
              let dx = fst pos2 - fst pos1 in
              let dy = snd pos2 - snd pos1 in
              sqrt ((float_of_int dx ** 2.) +. (float_of_int dy ** 2.))
            in
            distance pos wall1 <= float_of_int radius
            || distance pos wall2 <= float_of_int radius
          in
          (* We place the wall only when the player is near the emplacement of the wall that we want to place *)
          if is_near_wall opponent_pos (Option.get best_wall) near_radius then
            Some (Option.get best_wall)
          else None

(**
    This function gives a random move. We use this function when there is no path to the arrival (-> bug in bfs)    
    @param pos the current position of the player
    @return a random move
*)
let move_randomly (pos : position) : move =
  let possible_moves = list_of_moves pos in
  let random_index = Random.int (List.length possible_moves) in
  Moving (List.nth possible_moves random_index)

(**
    This function checks if a step is possible, if not, it gives a random move.
    - Why do we need this ?
    Sometimes, the bot will try to do a move that is not possible in some very specific cases.
    Explaining why it happens is very complicated, but it is not a big deal, because it happens very rarely.
    So we check if the move is possible, and if not, we return a random move. 
    @param pos the current position of the player
    @param step the step to do
    @return the move to do to reach the arrival    
*)
let move_random_if_not_possible (pos : position) (step : position) : move =
  let possible_moves = list_of_moves pos in
  if List.mem step possible_moves then Moving step
  else
    let () =
      print_string "\n <<< Servan avoided a wrong move : ";
      print_position step;
      print_string " >>>\n"
    in
    move_randomly pos

(**
    This function gives the move to do to reach the arrival
    @param pos the current position of the player
    @return the move to do to reach the arrival
*)
let move (pos : position) : move =
  (* We calculate the best path with bfs *)
  let initial_path =
    get_best_path_with_bfs (get_graph pos) pos wall_detection_radius
  in
  (* We take the first step *)
  let best_move = List.hd initial_path in
  (* We check if the move is possible *)
  move_random_if_not_possible pos best_move

(**
    This function represents my bot. Here is what it does :
    - We want to see if it's a good idea to place a wall now
    - If it is, we place it
    - If not, we move
    @param pos the current position of the player
    @return the move to do to
*)
let bot_yazici_servan (pos : position) : move =
  (* print_string "\n\n<<< Servan's turn : >>>"; *)
  let my_path =
    get_best_path_with_bfs (get_graph pos) pos wall_detection_radius
  in
  (* If we are one step away from victory, move, don't try to place a wall *)
  if List.length my_path = 1 then move pos
  else
    let wall = place_wall pos near_wall_scanner in
    if wall <> None then
      Placing_wall (fst (Option.get wall), snd (Option.get wall))
    else move pos
