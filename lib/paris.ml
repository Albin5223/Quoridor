open Types
open Board

(**Function to set a position in a board*)
let set_position (board : int list list) (position : position) (value : int) :
    int list list =
  let rec aux board acc i =
    match board with
    | [] -> acc
    | x :: xs ->
        if i = snd position then
          let rec aux2 x acc2 j =
            match x with
            | [] -> acc2
            | y :: ys ->
                if j = fst position then aux2 ys (acc2 @ [ value ]) (j + 1)
                else aux2 ys (acc2 @ [ y ]) (j + 1)
          in
          let new_board = aux2 x [] 0 in
          aux xs (acc @ [ new_board ]) (i + 1)
        else aux xs (acc @ [ x ]) (i + 1)
  in
  aux board [] 0

(*(start_pos, current_pos)list -> start_pos list * current_pos list*)

(**This function return a tuple 
   the first part of tuple is start_position in 
   the second part of tuple is current_position*)
let split () : position list * position list =
  let all_position = get_all_player_pos () in
  let rec aux_split (all_position : (position * position) list)
      (acc1 : position list) (acc2 : position list) =
    match all_position with
    | [] -> (acc1, acc2)
    | x :: xs -> aux_split xs (acc1 @ [ fst x ]) (acc2 @ [ snd x ])
  in
  aux_split all_position [] []

(**Function to compare two positions*)
let equal_position (position1 : position) (position2 : position) : bool =
  fst position1 = fst position2 && snd position1 = snd position2

(*
  even position, it's a player
  odd position, it's a wall
  if value = 0, it's empty
  if value = 1, it's a wall
  if value = 2, it's an opponent
  if value = 4, it's my player
  if value = 5, it's my target zone
*)

(**Function to create a board to manipulate a fictif board
    @param joueur position of all player
    @param wall list of position of walls in board
    @param my_player position of my player
    @param my_target_zone target zone 
    @return List of integer list that corresponds to the current board*)
let copy_board (joueur : position list) (wall : position list)
    (my_player : position) (my_target_zone : position list) : int list list =
  let board =
    List.init board_size (fun _ -> List.init board_size (fun _ -> 0))
  in
  let rec loop i j board =
    if i = board_size && j = board_size then board
    else if j = board_size then loop (i + 1) 0 board
    else
      let position = (i, j) in

      if equal_position position my_player then
        let new_board = set_position board position 4 in
        loop i (j + 1) new_board
      else if List.mem position joueur then
        let new_board = set_position board position 2 in
        loop i (j + 1) new_board
      else if List.mem position wall then
        let new_board = set_position board position 1 in
        loop i (j + 1) new_board
      else if List.mem position my_target_zone then
        let new_board = set_position board position 5 in
        loop i (j + 1) new_board
      else loop i (j + 1) board
  in
  loop 0 0 board

(*               PRETTY PRINT            *)

(*Pretty print for board*)
(*
let pretty_print_board (b : int list list) : unit =
  print_string "\n";
  print_string "\n";
  List.iter (fun x -> List.iter (fun y -> print_int y) x; print_string "\n") b

  *)

(*Pretty print for a position*)

let _pretty_print_position x =
  print_string "(";
  print_int (fst x);
  print_string ",";
  print_int (snd x);
  print_string ")";
  Format.printf "@."

(**This function returns a target zone 
   @param postion_start is the start position
   @return List of position to win*)
let get_list_of_position_to_win (position_start : position) : position list =
  let board_size = board_size in
  match position_start with
  | x, y when x = board_size / 2 && y = 0 ->
      List.init board_size (fun x -> (x, board_size - 1))
  | x, y when x = board_size / 2 && y = board_size - 1 ->
      List.init board_size (fun x -> (x, 0))
  | x, y when x = 0 && y = board_size / 2 ->
      List.init board_size (fun x -> (board_size - 1, x))
  | x, y when x = board_size - 1 && y = board_size / 2 ->
      List.init board_size (fun x -> (0, x))
  | _ -> failwith "position_start n'est pas une position de départ"

(**
  @param position a position
  @return return true if the position is out of the board*)
let is_out_of_board (position : position) : bool =
  let x, y = position in
  x < 0 || x >= board_size || y < 0 || y >= board_size

(**Function that checks if there is a player at the indicated position
    @param position to check
    @return true if there is a player in the indicated position*)
let is_position_contained_player (position : position) : bool =
  let _, current = split () in
  List.mem position current

(*Fonction qui permet de trouver toutes les positions adjacentes a position_voisin*)

(**Fonction auxiliaire qui saute le joueur voisin
    @param position_current current position of player
    @param position_voisin neightboring position of current player
    @return None if we can't jump the neighboring positon else Some x where x is a new position localized after neightboring 
      position in the same vector*)
let aux_make_jump (position_current : position) (position_voisin : position) :
    position option =
  let x1, y1 = position_current in
  let x2, y2 = position_voisin in
  if x1 = x2 then
    if y1 < y2 then
      let new_position = (x1, y2 + 2) in
      if
        is_out_of_board new_position
        || is_wall_between new_position position_voisin
      then None
      else if is_position_contained_player new_position then None
      else Some new_position
    else
      let new_position = (x1, y2 - 2) in
      if
        is_out_of_board new_position
        || is_wall_between new_position position_voisin
      then None
      else if is_position_contained_player new_position then None
      else Some new_position
  else if x1 < x2 then
    let new_position = (x2 + 2, y1) in
    if
      is_out_of_board new_position
      || is_wall_between new_position position_voisin
    then None
    else if is_position_contained_player new_position then None
    else Some new_position
  else
    let new_position = (x2 - 2, y1) in
    if
      is_out_of_board new_position
      || is_wall_between new_position position_voisin
    then None
    else if is_position_contained_player new_position then None
    else Some new_position

(**This function returns the list of neighbors of position_neighbor reachable by following the rules of the game
    @param position_current current position of player
    @param position_voisin neighboring position of player
    @return List of position reachable by following the rules of the gale*)
let make_jump (position_current : position) (position_voisin : position) :
    position list =
  let jump = aux_make_jump position_current position_voisin in
  match jump with
  | None ->
      let v1 = (fst position_voisin + 2, snd position_voisin) in
      let v2 = (fst position_voisin - 2, snd position_voisin) in
      let v3 = (fst position_voisin, snd position_voisin + 2) in
      let v4 = (fst position_voisin, snd position_voisin - 2) in
      let neightbor = [ v1; v2; v3; v4 ] in
      let neightbor =
        List.fold_left
          (fun acc x -> if is_out_of_board x then acc else x :: acc)
          [] neightbor
      in
      let neightbor =
        List.fold_left
          (fun acc x ->
            if is_wall_between position_voisin x then acc else x :: acc)
          [] neightbor
      in
      let neightbor =
        List.fold_left
          (fun acc x ->
            if equal_position position_current x then acc else x :: acc)
          [] neightbor
      in
      let neightbor =
        List.fold_left
          (fun acc x ->
            if is_position_contained_player x then acc else x :: acc)
          [] neightbor
      in
      neightbor
  | Some x -> [ x ]

(**This function give the distance between a position and a target zone
    @param position 
    @param target_zone
    @return int*)
let distance (position : position) (target_zone : position list) : int =
  let rec aux_distance (target_zone : position list) (min : int) : int =
    match target_zone with
    | [] -> min
    | x :: xs ->
        let x1, y1 = position in
        let x2, y2 = x in
        let distance = abs (x1 - x2) + abs (y1 - y2) in
        if distance < min then aux_distance xs distance else aux_distance xs min
  in
  aux_distance target_zone 10000

(**
    @param board
    @param position1
    @param position2
    @return true if the is a wall between position1 and position2*)
let is_wall_between (board : int list list) (position1 : position)
    (position2 : position) : bool =
  let x1, y1 = position1 in
  let x2, y2 = position2 in
  let wall_position = ((x1 + x2) / 2, (y1 + y2) / 2) in
  let wall =
    List.nth (List.nth board (snd wall_position)) (fst wall_position)
  in
  if wall = 1 then true else false

(**
    @param board
    @param current_position
    @param target_zone
    @param visited List of position already visited
    @return position list where position in this list aren't no visited and are the neighbor of current_position*)
let get_neightbor_no_visited (board : int list list)
    (current_position : position) (target_zone : position list)
    (visited : position list) =
  let v1 = (fst current_position + 2, snd current_position) in
  let v2 = (fst current_position - 2, snd current_position) in
  let v3 = (fst current_position, snd current_position + 2) in
  let v4 = (fst current_position, snd current_position - 2) in
  let neightbor = [ v1; v2; v3; v4 ] in

  let neightbor =
    List.fold_left
      (fun acc x -> if is_out_of_board x then acc else x :: acc)
      [] neightbor
  in
  let neightbor =
    List.fold_left
      (fun acc x -> if List.mem x visited then acc else x :: acc)
      [] neightbor
  in
  (*This function is goinf to check the neighbors of current_position, check : if there is not a wall between current_position and a neighbor
     or we can jump over the position because there is a player on it*)
  let rec verifNeightbor (board : int list list) (neightbor : position list)
      (acc : position list) =
    match neightbor with
    | [] -> acc
    | x :: xs ->
        if is_wall_between board current_position x then
          verifNeightbor board xs acc
        else if is_position_contained_player x then
          let jump = make_jump current_position x in
          verifNeightbor board xs (acc @ jump)
        else verifNeightbor board xs (acc @ [ x ])
  in
  let neightbor = verifNeightbor board neightbor [] in
  let neightbor =
    List.fold_left
      (fun acc x -> if is_out_of_board x then acc else x :: acc)
      [] neightbor
  in
  let neightbor =
    List.fold_left
      (fun acc x -> if List.mem x visited then acc else x :: acc)
      [] neightbor
  in
  let neightbor =
    List.sort
      (fun x y -> compare (distance x target_zone) (distance y target_zone))
      neightbor
  in
  neightbor

(**
    This function add new neightbor to the all path. If a path finish by current position, we remove it and we add the new path with the new neightbor
    @param all_paths 
    @param neighbor
    @param current
    @return new all path*)
let add_path (all_paths : position list list) (neightbor : position list)
    (current : position) : position list list =
  let path_finish_by_current =
    List.find
      (fun x ->
        let last = List.nth x (List.length x - 1) in
        equal_position last current)
      all_paths
  in
  let remove_path_finish_by_current =
    List.fold_left
      (fun acc x ->
        let last = List.nth x (List.length x - 1) in
        if equal_position last current then acc else acc @ [ x ])
      [] all_paths
  in
  let add_new_neightbor_to_path =
    List.map (fun x -> path_finish_by_current @ [ x ]) neightbor
  in
  List.append remove_path_finish_by_current add_new_neightbor_to_path

(**Auxiliary function to find the shortest path
    If the priority_queue is empty, we return all_path
    If the priority_queue is not empty, we take the first element of the priority_queue and we check if it's a target_zone
    If it's a target_zone, we return all_path
    Else we take the neighbors of the first element of the priority_queue and we add it to the priority_queue
    @param board
    @param priority_queue
    @param target_zone
    @param all_path
    @param visited
    @return all path*)
let rec aux_plus_court_chemin (board : int list list)
    (priority_queue : position list) (target_zone : position list)
    (all_path : position list list) (visited : position list) :
    position list list =
  match priority_queue with
  | [] -> all_path
  | x :: xs ->
      if List.mem x target_zone then all_path
      else
        let neightbor = get_neightbor_no_visited board x target_zone visited in
        if List.length neightbor = 0 then
          aux_plus_court_chemin board xs target_zone all_path visited
        else
          let new_proirity_queue = List.append xs neightbor in
          let new_all_path = add_path all_path neightbor x in
          aux_plus_court_chemin board new_proirity_queue target_zone
            new_all_path (neightbor @ visited)

(**This function launch aux_plus_court_chemin
    @param board 
    @param current_position
    @param start_position
    @return position list shortest path between current_position and target_zone*)
let plus_court_chemin (board : int list list) (current_position : position)
    (start_position : position) : position list =
  let target_zone = get_list_of_position_to_win start_position in
  let all_path =
    aux_plus_court_chemin board [ current_position ] target_zone
      [ [ current_position ] ] [ current_position ]
  in
  try
    let path =
      List.find
        (fun x ->
          let last = List.nth x (List.length x - 1) in
          List.mem last target_zone)
        all_path
    in
    path
  with _ ->
    let path =
      List.fold_left
        (fun acc x -> if List.length x > List.length acc then x else acc)
        [] all_path
    in
    path

type best_wall = { position1 : position; position2 : position }
(**New type to manipulate wall easier*)

type information = { current_position : position; start_position : position }

(**Constant which corresponds to the absence of information *)
let empty_information =
  { current_position = (-1, -1); start_position = (-1, -1) }

(**
    @param position
    @return true if in the position there is nothing*)
let is_free_position (position : position) : bool =
  let _, current = split () in
  let wall = get_all_wall_pos () in
  if List.mem position current then false
  else if List.mem position wall then false
  else true

(**
    @param position1
    @param positio2
    @return best_wall the wall between position1 and position2*)
let get_position_between (position1 : position) (position2 : position) :
    best_wall =
  let x1, y1 = position1 in
  let x2, y2 = position2 in
  let positions_wall1 = ((x1 + x2) / 2, (y1 + y2) / 2) in
  if y1 = y2 then
    let positions_wall2 = ((x1 + x2) / 2, ((y1 + y2) / 2) + 1) in
    if is_out_of_board positions_wall2 || not (is_free_position positions_wall2)
    then
      let positions_wall2 = ((x1 + x2) / 2, ((y1 + y2) / 2) - 1) in
      { position1 = positions_wall1; position2 = positions_wall2 }
    else { position1 = positions_wall1; position2 = positions_wall2 }
  else
    let positions_wall2 = (((x1 + x2) / 2) + 1, (y1 + y2) / 2) in
    if is_out_of_board positions_wall2 || not (is_free_position positions_wall2)
    then
      let positions_wall2 = (((x1 + x2) / 2) - 1, (y1 + y2) / 2) in
      { position1 = positions_wall1; position2 = positions_wall2 }
    else { position1 = positions_wall1; position2 = positions_wall2 }

(**Function to check if we can place a wall in position1 and position2
    @param position1
    @param postion2
    @return bool*)
let can_place_wall_in_position (position1 : position) (position2 : position) :
    bool =
  try
    let player = current_player () in
    let () = validate_wall_placement player.walls_left position1 position2 in
    true
  with _ -> false

(**This function find the best wall to block a player
    @param board
    @param current_positon
    @param start_position
    @return best_wall*)
let determine_best_wall_for_player (board : int list list)
    (current_position : position) (start_position : position) : best_wall =
  let path = plus_court_chemin board current_position start_position in
  let rec aux_determine_best_wall (path : position list) : best_wall =
    match path with
    | [] -> failwith "path est vide"
    | _ :: [] -> failwith "path ne contient qu'un seul element"
    | x :: y :: xs ->
        let position_between = get_position_between x y in
        if
          can_place_wall_in_position position_between.position1
            position_between.position2
        then position_between
        else aux_determine_best_wall ([ y ] @ xs)
  in
  aux_determine_best_wall path

(**This function put in a structure the information of opponent
    @param my_position
    @return information list*)
let get_all_player_information (my_position : position) : information list =
  let start, current = split () in
  let rec aux_get_all_player_information (start : position list)
      (current : position list) (acc : information list) =
    match (start, current) with
    | [], [] -> acc
    | x :: xs, y :: ys ->
        if equal_position y my_position then
          aux_get_all_player_information xs ys acc
        else
          aux_get_all_player_information xs ys
            (acc @ [ { current_position = y; start_position = x } ])
    | _ -> failwith "start et current n'ont pas la meme taille"
  in
  aux_get_all_player_information start current []

(**Function to check is my player can place a wall
    @param unit
    @return bool*)
let can_place_wall () : bool =
  let player = current_player () in
  player.walls_left > 0

(*        FUNCTION TO DECIDE IF WE GO OR WE PLACE A WALL      *)

(*Fonction qui detecte si un adversaire est en approche de sa zone*)

(*New type to represent an action*)
type action = Move | Place of best_wall

(**Function that returns an opponent's distance to their target area
    @param ennemie : information
    @return int*)
let is_ennemie_approach_aux (ennemie : information) : int =
  let wall = get_all_wall_pos () in
  let _, current = split () in
  let target_zone = get_list_of_position_to_win ennemie.start_position in
  let board = copy_board current wall ennemie.current_position target_zone in
  let path =
    plus_court_chemin board ennemie.current_position ennemie.start_position
  in
  let distance = List.length path in
  distance

(**Function that returns a list of opponents who are approaching their target area
    @param ennemie : information list
    @return information list option, None if there are no opponent who are approaching their target zone*)
let is_ennemie_approach (ennemie : information list) : information list option =
  let rec aux_approach (ennemie : information list) (acc : information list) =
    match ennemie with
    | [] -> acc
    | x :: xs ->
        let new_distance = is_ennemie_approach_aux x in
        if new_distance <= 4 then aux_approach xs ([ x ] @ acc)
        else aux_approach xs acc
  in
  let info = aux_approach ennemie [] in
  if List.length info = 0 then None else Some info

(**Function that checks if I am closer to winning than my opponents
    @param myself : information about my player
    @param information_ennemie : information about opponent(s)
    @retrun true if I am closer to winning*)
let is_myself_approach (myself : information)
    (information_ennemie : information list) : bool =
  let wall = get_all_wall_pos () in
  let _, current = split () in
  let target_zone = get_list_of_position_to_win myself.start_position in
  let board = copy_board current wall myself.current_position target_zone in
  let path =
    plus_court_chemin board myself.current_position myself.start_position
  in
  let value = List.length path in
  let rec aux (information : information list) : bool =
    match information with
    | [] -> true
    | x :: xs ->
        let target_zone = get_list_of_position_to_win x.start_position in
        let board = copy_board current wall x.current_position target_zone in
        let path =
          plus_court_chemin board x.current_position x.start_position
        in
        let new_value = List.length path in
        if new_value < value then false else aux xs
  in
  aux information_ennemie

(**Function that looks at which of my opponents is closest to winning
    @param ennemie : information about opponent(s)
    @return Some of x if there is a opponent else None If there is no opponent close to winning or if there are several*)
let opponent_approach_win (ennemie : information list) : information option =
  let opponent = is_ennemie_approach ennemie in
  match opponent with
  | None -> None
  | Some x ->
      let rec aux_approach (ennemie : information list) (acc : information)
          (draw : bool) =
        match ennemie with
        | [] -> if draw then None else Some acc
        | x :: xs ->
            let new_distance = is_ennemie_approach_aux x in
            let old_distance = is_ennemie_approach_aux acc in
            if new_distance < old_distance then aux_approach xs x false
            else if new_distance = old_distance then aux_approach xs acc true
            else aux_approach xs acc false
      in
      aux_approach x (List.hd x) false

(*Fonction qui regarde sur le wall qui va le plus allonger le chemin d'un adversaire*)

(**
    Function which finds the position of a wall which will extend as much as possible the shortest path of my opponents close to winning
    @param ennemie : information about opponent(s) close to winnig
    @return best_wall*)
let search_best_wall (ennemie : information list) : best_wall =
  let rec aux (ennemie : information list) (acc : best_wall) (value : int) :
      best_wall =
    match ennemie with
    | [] -> acc
    | x :: xs -> (
        let wall = get_all_wall_pos () in
        let _, current = split () in
        let target_zone = get_list_of_position_to_win x.start_position in
        let board = copy_board current wall x.current_position target_zone in
        let path =
          plus_court_chemin board x.current_position x.start_position
        in
        let new_value = List.length path in
        try
          let new_wall =
            determine_best_wall_for_player board x.current_position
              x.start_position
          in
          let new_board =
            copy_board current
              (new_wall.position1 :: new_wall.position2 :: wall)
              x.current_position target_zone
          in
          let new_path =
            plus_court_chemin new_board x.current_position x.start_position
          in
          let new_new_value = List.length new_path in
          let diff = new_new_value - new_value in
          if diff > value then aux xs new_wall diff else aux xs acc value
        with _ -> aux xs acc value)
  in
  aux ennemie { position1 = (-1, -1); position2 = (-1, -1) } 0

(*Fonction qui detecte que l'adversaire passe par un plus court chemin où je ne peux pas poser de mur*)

(**Function to detect if a opponent has a shortest path where I can't place wall in 75% of path
    @param ennemie : information about opponent(s)
    @return bool * information if true then information is information about a opponent where I can't place a wall in 75% of his path
    else false then information is empty information*)
let detect_no_possible_to_place_wall (ennemie : information list) :
    bool * information =
  let wall = get_all_wall_pos () in
  let _, current = split () in
  let rec aux (ennemie : information list) =
    match ennemie with
    | [] -> (false, empty_information)
    | x :: xs ->
        let target_zone = get_list_of_position_to_win x.start_position in
        let board = copy_board current wall x.current_position target_zone in
        let path =
          plus_court_chemin board x.current_position x.start_position
        in
        let emplacement_max = List.length path - 1 in
        let rec count_emplacement_possible (path : position list) (acc : int) =
          match path with
          | [] -> acc
          | _ :: [] -> acc
          | x :: xs ->
              let neightbor = List.hd xs in
              let position_between = get_position_between x neightbor in
              if
                can_place_wall_in_position position_between.position1
                  position_between.position2
              then count_emplacement_possible xs (acc + 1)
              else count_emplacement_possible xs acc
        in
        let emplacement_no_possible =
          emplacement_max - count_emplacement_possible path 0
        in
        let ratio =
          float_of_int emplacement_no_possible
          /. float_of_int emplacement_max
          *. 100.
        in
        if ratio > 75. then (true, x) else aux xs
  in
  aux ennemie

(**
    Function that chooses the best movement
    @param unit
    @return position*)
let move () : position =
  let my_player = current_player () in
  let _, current = split () in
  let wall = get_all_wall_pos () in
  let my_target_zone = get_list_of_position_to_win my_player.start_position in
  let board =
    copy_board current wall my_player.current_position my_target_zone
  in
  let path =
    plus_court_chemin board my_player.current_position my_player.start_position
  in
  List.nth path 1

(**Function that determines the action to be done
    @param information_ennemie : inforamtion about opponent(s)
    @return action to be done*)
let euristic (information_ennemie : information list) : action =
  let my_player = current_player () in
  if
    (not (can_place_wall ()))
    || is_myself_approach
         {
           current_position = my_player.current_position;
           start_position = my_player.start_position;
         }
         information_ennemie
  then Move
  else
    let detect, info = detect_no_possible_to_place_wall information_ennemie in
    if detect then
      let _, current = split () in
      let wall = get_all_wall_pos () in
      let target_zone = get_list_of_position_to_win info.start_position in
      let board = copy_board current wall info.current_position target_zone in
      try
        let wall =
          determine_best_wall_for_player board info.current_position
            info.start_position
        in
        Place wall
      with _ -> Move
    else
      let is_ennemie_approach = is_ennemie_approach information_ennemie in
      match is_ennemie_approach with
      | None -> Move
      | Some opponent_list -> (
          let opponent = opponent_approach_win opponent_list in
          match opponent with
          | None ->
              let wall = search_best_wall opponent_list in
              if
                equal_position wall.position1 (-1, -1)
                && equal_position wall.position2 (-1, -1)
              then Move
              else Place wall
          | Some x -> (
              let _, current = split () in
              let wall = get_all_wall_pos () in
              let target_zone = get_list_of_position_to_win x.start_position in
              let board =
                copy_board current wall x.current_position target_zone
              in
              try
                let wall =
                  determine_best_wall_for_player board x.current_position
                    x.start_position
                in
                Place wall
              with _ -> Move))

(*        Here is my bot             *)
let my_bot_paris_albin (position : position) : move =
  (*  print_string "\n\n Turn of Albin \n\n"; *)
  let information = get_all_player_information position in
  let action = euristic information in
  match action with
  | Move ->
      let move = move () in
      (* let () = print_string "move : " in
         let () = pretty_print_position move in*)
      Moving move
  | Place wall ->
      (* let () = print_string "wall : " in
         let () = pretty_print_position wall.position1 in
         let () = pretty_print_position wall.position2 in *)
      Placing_wall (wall.position1, wall.position2)
