open Board
open Types

let rec wall_ahead pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red ->
      if y = 16 then true
      else if is_wall_between pos (x, y + 2) then false
      else wall_ahead (x, y + 2)
  | Blue ->
      if y = 0 then true
      else if is_wall_between pos (x, y - 2) then false
      else wall_ahead (x, y - 2)
  | Green ->
      if x = 16 then true
      else if is_wall_between pos (x + 2, y) then false
      else wall_ahead (x + 2, y)
  | Yellow ->
      if x = 0 then true
      else if is_wall_between pos (x - 2, y) then false
      else wall_ahead (x - 2, y)

let rec wall_east pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red ->
      if x = 0 then true
      else if is_wall_between pos (x - 2, y) then false
      else wall_east (x - 2, y)
  | Blue ->
      if x = 16 then true
      else if is_wall_between pos (x + 2, y) then false
      else wall_east (x + 2, y)
  | Green ->
      if y = 0 then true
      else if is_wall_between pos (x, y - 2) then false
      else wall_east (x, y - 2)
  | Yellow ->
      if y = 16 then true
      else if is_wall_between pos (x, y + 2) then false
      else wall_east (x, y + 2)

let rec wall_west pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red ->
      if x = 16 then true
      else if is_wall_between pos (x + 2, y) then false
      else wall_west (x + 2, y)
  | Blue ->
      if x = 0 then true
      else if is_wall_between pos (x - 2, y) then false
      else wall_west (x - 2, y)
  | Green ->
      if y = 16 then true
      else if is_wall_between pos (x, y + 2) then false
      else wall_west (x, y + 2)
  | Yellow ->
      if y = 0 then true
      else if is_wall_between pos (x, y - 2) then false
      else wall_west (x, y - 2)

let gandalf pos = not (is_player pos)

let moving_forward pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red -> if not (gandalf (x, y + 2)) then (x, y + 4) else (x, y + 2)
  | Blue -> if not (gandalf (x, y - 2)) then (x, y - 4) else (x, y - 2)
  | Green -> if not (gandalf (x + 2, y)) then (x + 4, y) else (x + 2, y)
  | Yellow -> if not (gandalf (x - 2, y)) then (x - 4, y) else (x - 2, y)

let moving_right pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red -> (x - 2, y)
  | Blue -> (x + 2, y)
  | Green -> (x, y - 2)
  | Yellow -> (x, y + 2)

let moving_left pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red -> (x + 2, y)
  | Blue -> (x - 2, y)
  | Green -> (x, y + 2)
  | Yellow -> (x, y - 2)

let moving_backwards pos =
  let x, y = pos in
  let joueur = current_player () in
  match joueur.color with
  | Red -> (x, y - 2)
  | Blue -> (x, y + 2)
  | Green -> (x - 2, y)
  | Yellow -> (x + 2, y)

(** returns the best possible movement for the current player **)
let better_move pos =
  let lstMv = list_of_moves pos in
  match lstMv with
  | [] -> raise (NoMovePossible "There is no movement possible for this player")
  | _ ->
      if wall_ahead pos && List.mem (moving_forward pos) lstMv then
        Moving (moving_forward pos)
      else if wall_east pos && List.mem (moving_right pos) lstMv then
        Moving (moving_right pos)
      else if wall_west pos && List.mem (moving_left pos) lstMv then
        Moving (moving_left pos)
      else Moving (moving_backwards pos)

(** if the opponent is in a better position than the current player, places a wall to block him. If it isn't the case, applies
    the function better move **)
let best_move pos =
  let others = get_all_player_pos () in
  let opponent = List.nth others 1 in
  let y1 = snd pos in
  let x2, y2 = snd opponent in
  let wos = get_all_wall_pos () in
  if y2 - 8 > 17 - y1 && (current_player ()).walls_left > 0 then
    if x2 + 1 < 17 then
      if
        (not (List.mem (x2, y2 - 1) wos)) && not (List.mem (x2 + 1, y2 - 1) wos)
      then Placing_wall ((x2, y2 - 1), (x2 + 1, y2 - 1))
      else if
        (not (List.mem (x2, y2 - 1) wos)) && not (List.mem (x2 + 1, y2 - 1) wos)
      then Placing_wall ((x2, y2 - 1), (x2 + 1, y2 - 1))
      else better_move pos
    else better_move pos
  else better_move pos
