open Board
open Types

(* print_pair_list show all the movement possible for the turn*)
let _print_pair_list pairs =
  Printf.printf "\nla list des mouvements possible [";
  List.iter (fun (x, y) -> Printf.printf "(%d, %d)" x y) pairs;
  Printf.printf "]\n"

let avancer pos =
  let c = (current_player ()).color in
  match c with
  | Red ->
      let rec movear lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y > snd pos then (x, y) else movear h pos
        | [] -> pos
      in
      movear (list_of_moves pos) pos
  | Blue ->
      let rec moveab lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y < snd pos then (x, y) else moveab h pos
        | [] -> pos
      in
      moveab (list_of_moves pos) pos
  | Green ->
      let rec moveag lom pos =
        match lom with
        | (x, y) :: h ->
            if x > fst pos && y == snd pos then (x, y) else moveag h pos
        | [] -> pos
      in
      moveag (list_of_moves pos) pos
  | Yellow ->
      let rec moveay lom pos =
        match lom with
        | (x, y) :: h ->
            if x < fst pos && y == snd pos then (x, y) else moveay h pos
        | [] -> pos
      in
      moveay (list_of_moves pos) pos

let reculer pos =
  let c = (current_player ()).color in
  match c with
  | Red ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y < snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Blue ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y > snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Green ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x < fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Yellow ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x > fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos

let decalergauche pos =
  let c = (current_player ()).color in
  match c with
  | Red ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x > fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Blue ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x < fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Green ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y < snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Yellow ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y > snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos

let decalerdroite pos =
  let c = (current_player ()).color in
  match c with
  | Red ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x < fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Blue ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x > fst pos && y == snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Green ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y > snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos
  | Yellow ->
      let rec move lom pos =
        match lom with
        | (x, y) :: h ->
            if x == fst pos && y < snd pos then (x, y) else move h pos
        | [] -> pos
      in
      move (list_of_moves pos) pos

let rec chemdroite dist pos =
  if List.mem (avancer pos) (list_of_moves pos) then dist
  else if List.mem (decalerdroite pos) (list_of_moves pos) then
    chemdroite (dist + 1) (decalerdroite pos)
  else 100

let rec chemgauche dist pos =
  if List.mem (avancer pos) (list_of_moves pos) then dist
  else if List.mem (decalergauche pos) (list_of_moves pos) then
    chemgauche (dist + 1) (decalergauche pos)
  else 100

(*bot_move is the function that decide in which direction the player move*)
let bot_move pos =
  if avancer pos != pos then Moving (avancer pos)
  else if
    List.mem (decalergauche pos) (list_of_moves pos)
    || List.mem (decalerdroite pos) (list_of_moves pos)
  then
    if chemgauche 0 pos > chemdroite 0 pos then Moving (decalerdroite pos)
    else Moving (decalergauche pos)
  else Moving (reculer pos)

(* bot_play play the move for the bot *)
let bot_play pos =
  (*
  let () = print_pair_list (list_of_moves pos) in
  let () = Printf.printf "bot play\n" in *)
  bot_move pos
