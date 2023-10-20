(* Module contenant la logique du jeu, la victoire, les mouvements, ... *)
open Types

let posIsForPlayer = function XY(x,y) -> x mod 2 == 0 && y mod 2 == 0

let wallIsWellPlaced w = let (p1,p2) = w in if posIsForPlayer p1 || posIsForPlayer p2 then false else true