(* Module contenant la logique du jeu, la victoire, les mouvements, ... *)
open Board
open Types

let move game player = let lstMv = listOfMoves (player.position) in
    let r = Random.int (List.length lstMv) in let newPos = List.nth lstMv r in 
    changePosOfPlayer game newPos
      

let placeWall game player = let r = Random.int 2 in
if (r == 0) then (placeVerticalWall game player.position)
else (placeHorizontalWall game player.position)

let detMove game player =
  let r = Random.int 2 in 
    if (r == 0) then (move game player)
    else placeWall game player