(* Module contenant la logique du jeu, la victoire, les mouvements, ... *)
open Board
open Types

let move game player = let lstMv = list_of_moves (player.position) in
    let r = Random.int (List.length lstMv) in let newPos = List.nth lstMv r in 
    change_pos_of_player game newPos
      

let place_wall game player = let r = Random.int 2 in
if (r == 0) then (place_vertical_wall game player.position)
else (place_horizontal_wall game player.position)

let det_move game player =
  let r = Random.int 2 in 
    if (r == 0) then (move game player)
    else place_wall game player