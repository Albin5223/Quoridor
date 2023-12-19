open Types
open Board

let est_avant_red other_pos my_pos =
  snd other_pos
  < snd my_pos (*return true si le joueur rouge est avant le bleu*)

let est_avant_blue other_pos my_pos = snd other_pos > snd my_pos
let made_half_red pos = snd pos < board_size / 2
let made_half_blue pos = snd pos > board_size / 2

let avancer_red pos lstmv =
  if List.mem (fst pos, snd pos + 2) lstmv then
    List.mem (fst pos, snd pos + 4) (list_of_moves (fst pos, snd pos + 2))
    || snd pos + 2 == 17
  else false

let avancer_blue pos lstmv =
  if List.mem (fst pos, snd pos - 2) lstmv then
    List.mem (fst pos, snd pos - 4) (list_of_moves (fst pos, snd pos - 2))
    || snd pos - 2 = 0
  else false

(*let can_block_red pos my_pos =
  avancer_red pos (list_of_moves pos) && est_avant_red pos my_pos*)
let can_block_red pos = 
  avancer_red pos (list_of_moves pos) && not (is_wall (fst pos, snd pos + 1)) && not (is_wall (fst pos + 1, snd pos + 1))

(*let can_block_blue pos my_pos =
  avancer_blue pos (list_of_moves pos) && est_avant_blue pos my_pos*)
let can_block_blue pos =
  avancer_blue pos (list_of_moves pos) && not (is_wall (fst pos, snd pos - 1)) && not (is_wall (fst pos - 1, snd pos - 1))

let bot_pos_wall_red pos =
  Placing_wall ((fst pos, snd pos + 1), (fst pos + 1, snd pos + 1))

let bot_pos_wall_blue pos =
  Placing_wall ((fst pos, snd pos - 1), (fst pos - 1, snd pos - 1))

let chem_droit_red pos =
  let rec aux pos dist =
    let lstmv = list_of_moves pos in
    if not (List.mem (fst pos - 2, snd pos) lstmv) then None
    else if avancer_red pos lstmv then Some dist
    else aux (fst pos - 2, snd pos) (dist + 1)
  in
  aux pos 0

let chem_gauche_red pos =
  let rec aux pos dist =
    let lstmv = list_of_moves pos in
    if not (List.mem (fst pos + 2, snd pos) lstmv) then None
    else if avancer_red pos lstmv then Some dist
    else aux (fst pos + 2, snd pos) (dist + 1)
  in
  aux pos 0

let bot_move_red pos lstmv =
  if avancer_red pos lstmv then Moving (fst pos, snd pos + 2)
  else
    let g = chem_gauche_red pos in
    let d = chem_droit_red pos in
    match (g, d) with
    | None, None -> Moving (fst pos, snd pos - 2) (*reculer*)
    | None, Some _ -> Moving (fst pos - 2, snd pos) (*aller à droite*)
    | Some _, None -> Moving (fst pos + 2, snd pos) (*aller à gauche*)
    | Some dist_g, Some dist_d ->
        if dist_g < dist_d then Moving (fst pos + 2, snd pos) (*aller à gauche*)
        else Moving (fst pos - 2, snd pos)
(*aller à droite*)

let chem_droit_blue pos =
  let rec aux pos dist =
    let lstmv = list_of_moves pos in
    if not (List.mem (fst pos + 2, snd pos) lstmv) then None
    else if avancer_blue pos lstmv then Some dist
    else aux (fst pos + 2, snd pos) (dist + 1)
  in
  aux pos 0

let chem_gauche_blue pos =
  let rec aux pos dist =
    let lstmv = list_of_moves pos in
    if not (List.mem (fst pos - 2, snd pos) lstmv) then None
    else if avancer_blue pos lstmv then Some dist
    else aux (fst pos - 2, snd pos) (dist + 1)
  in
  aux pos 0

let bot_move_blue pos lstmv =
  if avancer_blue pos lstmv then Moving (fst pos, snd pos - 2)
  else
    let g = chem_gauche_blue pos in
    let d = chem_droit_blue pos in
    match (g, d) with
    | None, None -> Moving (fst pos, snd pos + 2) (*reculer*)
    | None, Some _ -> Moving (fst pos + 2, snd pos) (*aller à droite*)
    | Some _, None -> Moving (fst pos - 2, snd pos) (*aller à gauche*)
    | Some dist_g, Some dist_d ->
        if dist_g < dist_d then Moving (fst pos - 2, snd pos) (*aller à gauche*)
        else Moving (fst pos + 2, snd pos)
(*aller à droite*)

let get_red_pos () = List.nth (get_all_player_pos ()) 0
let get_blue_pos () = List.nth (get_all_player_pos ()) 1

let det_move_lea pos =
  let lstmv = list_of_moves pos in
  match (current_player ()).color with
  | Red ->
      let blue_pos = snd (get_blue_pos ()) in
      if made_half_blue blue_pos && can_block_blue blue_pos then
        bot_pos_wall_blue blue_pos
      else if lstmv <> [] then
        bot_move_red (current_player ()).current_position lstmv
      else bot_pos_wall_red (snd (get_blue_pos ()))
  | Blue ->
      let red_pos = snd (get_red_pos ()) in
      if made_half_red red_pos && can_block_red red_pos then
        bot_pos_wall_red red_pos
      else if lstmv <> [] then
        bot_move_blue (current_player ()).current_position lstmv
      else bot_pos_wall_blue (snd (get_red_pos ()))
  | _ ->
      raise
        (InvalidPlayerColor
           ((current_player ()).color, "Only two players for this bot"))
