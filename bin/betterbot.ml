open Quoridor.Board
open Quoridor.Types

let color_to_final_pos color =
  match color with
  | Red -> ('y', 16) (*16 en y*)
  | Blue -> ('y', 0) (*0 en y*)
  | Green -> ('x', 16) (*16 en x*)
  | Yellow -> ('x', 0)
(*0 en x*)

let have_all_player_in_board () =
  let rec aux (i, j) acc =
    match (i, j) with
    | 16, 16 -> acc
    | i, 16 ->
        if is_player (i, j) then aux (i + 2, 0) ((i, j) :: acc)
        else aux (i + 2, 0) acc
    | i, j ->
        if is_player (i, j) then aux (i, j + 2) ((i, j) :: acc)
        else aux (i, j + 2) acc
  in
  aux (0, 0) []

let test_wall_y pos =
  match pos with
  | x, y -> (
      try
        validate_wall_placement (current_player ()).walls_left (x, y) (x, y + 1);
        Some ((x, y), (x, y + 1))
      with
      | InvalidWallPlacement (_, _, _) -> None
      | _ -> None)

let test_wall_x pos =
  match pos with
  | x, y -> (
      try
        validate_wall_placement (current_player ()).walls_left (x, y) (x + 1, y);
        Some ((x, y), (x + 1, y))
      with
      | InvalidWallPlacement (_, _, _) -> None
      | _ -> None)

let list_of_wall () =
  let rec aux (i, j) acc =
    match (i, j) with
    | 16, 16 -> acc
    | i, 16 -> (
        match (test_wall_y (i, j), test_wall_x (i, j)) with
        | Some a, Some b -> aux (i + 1, 0) (a :: b :: acc)
        | Some a, None -> aux (i + 1, 0) (a :: acc)
        | None, Some b -> aux (i + 1, 0) (b :: acc)
        | None, None -> aux (i + 1, 0) acc)
    | i, j -> (
        match (test_wall_y (i, j), test_wall_x (i, j)) with
        | Some a, Some b -> aux (i, j + 1) (a :: b :: acc)
        | Some a, None -> aux (i, j + 1) (a :: acc)
        | None, Some b -> aux (i, j + 1) (b :: acc)
        | None, None -> aux (i, j + 1) acc)
  in
  aux (0, 0) []

let distance final_line (x, y) =
  match final_line with
  | 'x', value -> abs (x - value)
  | 'y', value -> abs (y - value)
  | _ -> 0

let in_final_line (x, y) final_line =
  match final_line with
  | 'x', value -> x = value
  | 'y', value -> y = value
  | _ -> false

let list_min lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | h :: q -> if h < acc then aux q h else aux q acc
  in
  aux lst 1000

let find_path final_line (x, y) =
  let rec aux (x, y) acc =
    if acc > 14 then 64 (*7 for the horizontal 7 for the vertical*)
    else
      match (x, y) with
      | x, y when in_final_line (x, y) final_line -> acc
      | x, y ->
          let lst = list_of_moves (x, y) in
          let rec aux2 lst acc new_pos =
            match lst with
            | [] -> new_pos
            | (x, y) :: q ->
                let value = distance final_line (x, y) in
                if value < acc then aux2 q value (x, y) else aux2 q acc new_pos
          in
          let xnew, ynew = aux2 lst max_int (x, y) in
          aux (xnew, ynew) (acc + 1)
  in

  aux (x, y) 0

let rec more_complex_path final_line (x, y) acc already_explored =
  if acc > 8 then 64
    (*dans le cas ou il a besoin d'aller derrière lui (si < 8 il peut faire le même mouvement en boucle)*)
  else if in_final_line (x, y) final_line then acc
  else
    let value_simple = find_path final_line (x, y) in
    if value_simple < 64 then value_simple + acc
    else
      let lst =
        let rec move_you_can_do moves =
          match moves with
          | [] -> []
          | (x', y') :: q ->
              if List.mem (x', y') already_explored then move_you_can_do q
              else (x', y') :: move_you_can_do q
        in
        move_you_can_do (list_of_moves (x, y))
      in
      list_min
        (List.map
           (fun (x, y) ->
             more_complex_path final_line (x, y) (acc + 1)
               (already_explored @ [ (x, y) ]))
           lst)

let value_of_a_move (x, y) =
  let final_line = color_to_final_pos (current_player ()).color in
  more_complex_path final_line (x, y) 0 []

let test_move_with_the_wall (x, y) (x', y') (x1, y1) (x2, y2) =
  if
    ((x + x') / 2, (y + y') / 2) = (x1, y1)
    || ((x + x') / 2, (y + y') / 2) = (x2, y2)
  then true
  else false

let find_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2) =
  let rec aux (x, y) acc =
    if acc > 14 then 64 (*7 for the horizontal 7 for the vertical*)
    else
      match (x, y) with
      | x, y when in_final_line (x, y) final_line -> acc
      | x, y ->
          let lst =
            let rec move_you_can_do_with_the_wall moves =
              match moves with
              | [] -> []
              | (x', y') :: q ->
                  if test_move_with_the_wall (x, y) (x', y') (x1, y1) (x2, y2)
                  then move_you_can_do_with_the_wall q
                  else (x', y') :: move_you_can_do_with_the_wall q
            in
            move_you_can_do_with_the_wall (list_of_moves (x, y))
          in

          let rec aux2 lst acc new_pos =
            match lst with
            | [] -> new_pos
            | (x, y) :: q ->
                let value = distance final_line (x, y) in
                if value < acc then aux2 q value (x, y) else aux2 q acc new_pos
          in
          let xnew, ynew = aux2 lst max_int (x, y) in
          aux (xnew, ynew) (acc + 1)
  in
  aux (x, y) 0

(*Parcours en largeur*)
let rec more_complex_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2) acc
    already_explored =
  if in_final_line (x, y) final_line then acc
  else if acc > 7 then 64 (*prediction de 7 coups a l'avance*)
  else if in_final_line (x, y) final_line then acc
  else
    let value_simple =
      find_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2)
    in
    if value_simple < 64 then value_simple + acc
    else
      let lst =
        let rec move_you_can_do_with_the_wall moves =
          match moves with
          | [] -> []
          | (x', y') :: q ->
              if
                test_move_with_the_wall (x, y) (x', y') (x1, y1) (x2, y2)
                || List.mem (x', y') already_explored
              then move_you_can_do_with_the_wall q
              else (x', y') :: move_you_can_do_with_the_wall q
        in
        move_you_can_do_with_the_wall (list_of_moves (x, y))
      in
      list_min
        (List.map
           (fun (x, y) ->
             more_complex_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2)
               (acc + 1)
               (already_explored @ [ (x, y) ]))
           lst)

let value_of_the_ennemy_move have_list_of_player =
  (*on cherche le joueur le plus près de la ligne d'arrivée*)
  let rec find_the_distance_player_most_on_final_line lst acc =
    match lst with
    | [] -> acc
    | (x, y) :: q ->
        let value =
          distance
            (color_to_final_pos
               (try get_color (x, y) with _ -> (current_player ()).color))
            (x, y)
        in
        if value <= acc then find_the_distance_player_most_on_final_line q value
        else find_the_distance_player_most_on_final_line q acc
  in
  let int_distance =
    find_the_distance_player_most_on_final_line have_list_of_player max_int
  in
  let rec find_the_list_of_player_most_on_the_final_line lst acc color =
    match lst with
    | [] -> color
    | (x, y) :: q ->
        let value =
          distance
            (color_to_final_pos
               (try get_color (x, y) with _ -> (current_player ()).color))
            (x, y)
        in
        if value <= acc then
          find_the_list_of_player_most_on_the_final_line q value color
          @ [ (try get_color (x, y) with _ -> (current_player ()).color) ]
        else find_the_list_of_player_most_on_the_final_line q acc color
  in
  let color_list =
    find_the_list_of_player_most_on_the_final_line have_list_of_player
      int_distance []
  in
  Random.self_init ();
  (*si il y en a plusieurs a la même distance on en prend un au hasard*)
  let color = List.nth color_list (Random.int (List.length color_list)) in
  let color =
    match color with
    | color when color = (current_player ()).color -> (
        match color with
        | Red -> Blue
        | Blue -> Red
        | Green -> Yellow
        | Yellow -> Green)
    | _ -> color
  in
  let final_line = color_to_final_pos color in
  let find_the_pos_of_the_color list =
    (*on cherche la position de la couleur qu'on a choisi*)
    match
      (List.find_opt (fun (x, y) ->
           (try get_color (x, y) with _ -> (current_player ()).color) = color))
        list
    with
    | Some p -> p
    | None -> List.hd list
    (*find avant retournais Not_found toutes les centaines de games *)
  in
  let x, y = find_the_pos_of_the_color have_list_of_player in
  (more_complex_path final_line (x, y) 0 [], color)

let value_of_the_ennemy_move_with_the_wall (x1, y1) (x2, y2) player_choose
    have_list_of_player =
  let final_line = color_to_final_pos player_choose in
  let find_the_pos_of_the_color list =
    match
      (List.find_opt (fun (x, y) ->
           (try get_color (x, y) with _ -> (current_player ()).color)
           = player_choose))
        list
    with
    | Some p -> p
    | None -> List.hd list
    (*find avant retournais Not_found toutes les centaines de games *)
  in
  let x, y = find_the_pos_of_the_color have_list_of_player in
  let value = find_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2) in
  (*on calcule une première fois simpelement*)
  if value < 64 then value (*si on trouve pas le chemin*)
  else
    more_complex_path_with_a_wall final_line (x, y) (x1, y1) (x2, y2) 0
      [ (x, y) ]

let will_wall_block_player_for_bot all_player_pos pos_of_my_wall pos_of_my_wall2
    =
  (*Morceau de code de board.ml réarrangé pour éviter le bug décris plus bas*)
  List.exists
    (fun pos ->
      List.for_all
        (fun pos_vect ->
          (pos_vect = pos_of_my_wall || pos_vect = pos_of_my_wall2)
          ||
          try is_wall (fst pos + fst pos_vect, snd pos + snd pos_vect)
          with InvalidPosition _ -> false)
        move_vectors)
    all_player_pos

let test_move pos =
  let lstMv = list_of_moves pos in
  (*si on peut déjà arriver a la fin pas besoin de calculer*)
  match
    List.find_opt
      (fun (x, y) ->
        in_final_line (x, y) (color_to_final_pos (current_player ()).color))
      lstMv
  with
  | Some (x, y) -> Moving (x, y)
  | None ->
      let lstWall = list_of_wall () in
      (*ici on test tout les coups possible et leur gain*)
      let rec aux test_all_moves acc value_of_acc =
        match test_all_moves with
        | [] -> (acc, value_of_acc)
        | (x, y) :: q ->
            let value_of_move = value_of_a_move (x, y) in
            if value_of_move < value_of_acc then aux q (x, y) value_of_move
            else aux q acc value_of_acc
      in
      let (x, y), interest_of_the_move = aux lstMv pos max_int in
      if (current_player ()).walls_left = 0 then Moving (x, y)
      else
        let have_list_of_player = have_all_player_in_board () in

        (*on calcule le gain calculé plus haut pour nous pour le joueur le plus proche de la ligne d'arrivée*)
        let value_of_the_ennemy_move, player_choose =
          value_of_the_ennemy_move have_list_of_player
        in

        (*sur toutes les position de wall permises on cherche la plus intéressantes*)
        let rec aux2 test_all_wall acc value_of_acc =
          match test_all_wall with
          | [] -> (acc, value_of_acc)
          (*on fait la différence entre avec et sans le wall fictif*)
          | ((x1, y1), (x2, y2)) :: q ->
              let value_of_wall =
                value_of_the_ennemy_move_with_the_wall (x1, y1) (x2, y2)
                  player_choose have_list_of_player
                - value_of_the_ennemy_move
              in
              if value_of_wall > value_of_acc then
                aux2 q ((x1, y1), (x2, y2)) value_of_wall
              else aux2 q acc value_of_acc
        in
        let ((x1, y1), (x2, y2)), interest_of_the_wall =
          aux2 lstWall ((-1, -1), (-1, -1)) 0
        in
        if interest_of_the_wall = 0 then Moving (x, y)
        else if
          (*on redimensionne la value du move pour qu'elle soit en rapport avec la value du wall*)
          (distance (color_to_final_pos (current_player ()).color) pos / 2)
          - interest_of_the_move
          > interest_of_the_wall
        then Moving (x, y)
        else if
          (*ici on test le bug dans board.ml qui raise (Wall placement blocks a player's path to goal) alors que ce n'est pas dans validate_wall_placement *)
          will_wall_block_player_for_bot
            (have_all_player_in_board ())
            (x1, y1) (x2, y2)
        then Moving (x, y)
        else Placing_wall ((x1, y1), (x2, y2))

(* juste un lien pour le nom de la stratégie*)
let better_move pos = test_move pos
