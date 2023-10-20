(* Spécification de la structure du plateau de jeu et de ses caractéristiques; fonctions liées à la vérification des déplacements, à l'affichage... *)
open Types

let boardSize = 17
let lstWall = []

let posX = function 
|XY(x,_) -> x 

let posY = function 
|XY(_,y) -> y


let wallClose p w = let (p1, p2) = w in let x = posX p in let y = posY p in
 let x1 = posX p1 in let y1 = posY p1 in
  if((x+1 == x1 && y+1 == y1)|| (x+1 == x1 && y-1 == y1)  || (x-1 == x1 && y+1 == y1) || (x-1 == x1 && y-1 == y1) ) then true
  else let x2 = posX p2 in let y2 = posY p2 in
    if((x+1 == x2 && y+1 == y2)|| (x+1 == x2 && y-1 == y2)  || (x-1 == x2 && y+1 == y2) || (x-1 == x2 && y-1 == y2) ) then true
    else false


(*
let lstOfMoves pl1 pl2 = 
  definir selon si il y a a coté un/une:
  - mur
  -extremite
  -joueur
  -rien
  *)