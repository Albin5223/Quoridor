open Board
open Types


(* lisibility for the code *)
type finishline = N | S | W | E

let marked_case = ref []
let pastx = ref 0
let pasty = ref 0

(* useless (it was to have the number of turn)*)
let cpt = ref 0


(* usefull to do recursion and avoir the exception as InvalidMove *)
let isInside (x,y) = x >= 0 && x <= 16 && y >= 0 && y <= 16 

(* more lisibilyty (here is an example the player who start on middle top positon need to finish to the sud )*)
let convertStartToFinishline (p : player) : finishline =
  let x, y = p.start_position in
  if x == board_size / 2 && y = 0 then S
  else if x = board_size / 2 && y == board_size - 1 then N
  else if x = 0 && y = board_size / 2 then E
  else W


(* in order to have the finish line depends on your start postion *)
let finishLinePerPlayer (p : player) : position list =
  match convertStartToFinishline p with
  | S -> List.init 8 (fun x' -> (2 + (x' * 2), board_size - 1))
  | N -> List.init 8 (fun x' -> (2 + (x' * 2), 0))
  | E -> List.init 8 (fun y' -> (board_size - 1, 2 + (y' * 2)))
  | W -> List.init 8 (fun y' -> (0, 2 + (y' * 2)))

let finishLine ()= finishLinePerPlayer (current_player ())

(* bunch of list of vectors (wall or direction) to do recusion the moves or the postion of walls : *)
(* ------ : *)

(* base for vectors *)
(* in priority a player goes :
   - straight 
   - right 
   - back 
   - left
   *)
let make_vectors_lists p t =
  match convertStartToFinishline p with
  | S -> [(0, t);(-t, 0);(0, -t);(t, 0)]
  | N -> [(0, -t);(t, 0);(0, t);(-t, 0)]
  | E -> [(t, 0); (0, t);(-t, 0);(0, -t)] 
  | W -> [(-t, 0);(0, -t);(t, 0);(0, t);] 


let vectorsDirection () = make_vectors_lists (current_player ()) 2

(* same as make_vectors_lists but with walls *)
let vectors_good_wall_list () =
  let (x,y) = (current_player()).current_position in
  match convertStartToFinishline (current_player()) with
  | S -> [ ((x,y-1),(x+1,y-1)); ((x,y-1),(x-1,y-1)); ((x-1,y-1),(x-2,y-1)); ((x+1,y-1),(x+2,y-1)); ((x+2,y),(x+2,y-1)); ((x-2,y),(x-2,y-1));]
  | N -> [ ((x,y+1),(x+1,y+1)); ((x,y+1),(x-1,y+1)); ((x-1,y+1),(x-2,y+1)); ((x+1,y+1),(x+2,y+1)); ((x+2,y),(x+2,y+1)); ((x-2,y),(x-2,y+1));]
  | W -> [ ((x+1,y),(x+1,y+1)); ((x+1,y),(x+1,y-1)); ((x+1,y-1),(x+1,y-2)); ((x+1,y+1),(x+1,y+2)); ((x,y+2),(x+1,y+2)); ((x,y-2),(x+1,y-2));]
  | E -> [ ((x-1,y),(x-1,y+1)); ((x-1,y),(x-1,y-1)); ((x-1,y-1),(x-1,y-2)); ((x-1,y+1),(x-1,y+2)); ((x,y+2),(x-1,y+2)); ((x,y-2),(x-1,y-2));]



(* make quoridors *)
let vectorsQuoridors () =
  match convertStartToFinishline (current_player ()) with
  | N -> [((9, 1),(9, 2)); ((9, 3),(9, 4)); ((9, 5),(9, 6)); ((9, 7),(9, 8)); ((8, 9),(9, 9)); ((6, 9),(7, 9)); ((4, 9),(5, 9)); ((2, 9),(3, 9)); ((0, 9),(1, 9));]
  | W -> [((1, 7),(2, 7)); ((3, 7),(4, 7)); ((5, 7),(6, 7)); ((7, 7),(8, 7)); ((9, 7),(9, 8)); ((9, 9),(9, 10));((9, 11),(9, 12));((9, 13),(9, 14));((9, 15),(9, 16));]
  | S -> [((7, 15),(7, 14)); ((7, 13),(7, 12)); ((7, 11),(7, 10)); ((7, 9),(7, 8)); ((7, 7),(8, 7)); ((9, 7),(10, 7)); ((11, 7),(12, 7)); ((13, 7),(14, 7));((15, 7),(16, 7));]
  | E -> [((15, 9),(14, 9)); ((13, 9),(12, 9)); ((11, 9),(10, 9)); ((9, 9),(8, 9)); ((7, 9),(7, 8));((7, 6),(7, 7));((7, 4),(7, 5));((7, 2),(7, 3));((7, 0),(7, 1));]
  
     
(* ----end---  *)



(* test if a player can place one of these wall (cf ligne 69) *)
let placeWallQuoridor () = 
  let vectors_q = vectorsQuoridors () in 
  let rec aux  i =
    try 
    if i = List.length vectors_q  then None
    else 
    let (p1,p2) = List.nth vectors_q i in
    validate_wall_placement ((current_player ()).walls_left) p1 p2;
    Some (p1,p2)
  with 
  | _ -> aux (i+1)
in aux 0


(* test if a player can finish (i.e contains the finish line) *)
let containsFinisLine path =
  let finish = finishLine () in
  let rec aux l =
    match l with
    | [] -> None
    | (px, py) :: xs ->
        if List.exists (fun (x', y') -> x' = px && y' = py) finish then Some(px, py)
        else aux xs
  in
  aux path 


(* test if a position is in direction vectors (cf ligne 48) *)
let respect_prio (x,y) (new_x,new_y) (vxi,vyi) =
  let rec modulo k = 
    if k = 4 then false
    else (
      if x+vxi*k=new_x && y+vyi*k=new_y then true
      else modulo(k+1))
  in modulo 1


(* returns list of (colors*postion) *)
let get_list_of_player () = 
  let rec aux i j l =
    if i = 17 then aux 0 (j+1) l
    else if j= 17 then l
    else 
      try 
        if get_color (i,j) <>  (current_player()).color then aux (i+1) j (l@[(get_color (i,j),(i,j))])
        else aux (i+1) j l
      with 
      | _ -> aux (i+1) j l 
    in aux 0 0 []


  
let is_only_one_direction (x,y) = 
  let rec aux i c = 
    match i with
    |0 -> if isInside (x+2,y) && not(is_wall (x+2,y)) then aux (i+1) (c+1) else  aux (i+1) c 
    |1 -> if isInside (x-2,y) && not(is_wall (x-2,y)) then aux (i+1) (c+1) else  aux (i+1) c 
    |2 -> if isInside (x,y+2) && not(is_wall (x,y+2)) then aux (i+1) (c+1) else  aux (i+1) c 
    |3 -> if isInside (x,y-2) && not(is_wall (x,y-2)) then aux (i+1) (c+1) else  aux (i+1) c 
    |_ -> c = 3
    in aux 0 0


(* fonction to move with list_of_moves *)
let can_move (x,y) =
  let lst_mv = list_of_moves (x,y) in
  let rec aux mv i (vxi,vyi) = 
    match mv with
    |[] -> 
      if (i+1) != 4 then ( aux lst_mv (i+1) (List.nth (vectorsDirection ()) (i+1)))
      else (
      let len = List.length lst_mv in 
      let r = Random.int len in
      List.nth lst_mv r)
    |(new_x,new_y)::xs -> 
      (
      let rec already_visited fst lst = match lst with
      |[] -> ()
      |((a,b),blocked)::xs -> if (a = x && b = y) then marked_case := (fst@[((!pastx,!pasty),true)]@xs)
      else already_visited (fst@[((a,b),blocked)]) xs 
    in already_visited [] (!marked_case);

    );
    match containsFinisLine lst_mv with
    |Some p -> p
    |None ->
      let b = respect_prio (x,y) (new_x,new_y) (vxi,vyi) in 
      if b && not (List.exists (fun ((a,b),blocked) -> (a = new_x && b = new_y && blocked=true)) !marked_case) then (
        (let b = is_only_one_direction (x,y) in marked_case := (!marked_case@[((x,y),b)]););
        (new_x,new_y))

      else aux xs i (vxi,vyi)
    in aux lst_mv 0 (List.nth (vectorsDirection ()) 0)


(* 
   calculate distance of opponent & 
   test if I need to block with my wall left
   
*)    
let need_to_block lst_ply = 
  if (current_player ()).walls_left = 0 then (false,(-1,-1),(-1,-1))
  else (
    let rec who_focus (p_focus:position) (d_min:int) l : (position*int) = (* with 4 players *)
      match l with
      |[] -> (p_focus,d_min)
      |(_,(x1,y1))::xs ->
      let d = (
        
        match convertStartToFinishline (current_player ()) with
        |S -> y1 
        |N -> board_size-1 - y1
        |W -> board_size-1 - x1
        |E -> x1) in
        if d < d_min then who_focus ((x1,y1)) d xs else who_focus p_focus d_min xs 
    in let (_,d) = (who_focus (999,999) 200 lst_ply) in 
    if (d/2) <= 5 then ( (* if he needs 5 turn to finish i need to try to block him *)
      let l = vectors_good_wall_list () in
        let rec posWall i = 
          if i = List.length l  then (false,(-1,-1),(-1,-1))
          else 
            try 
              let p1,p2 = List.nth l i in (
                validate_wall_placement ((current_player ()).walls_left) p1 p2; 
                (true,p1,p2))
          with 
          |_ -> posWall (i+1)
        in posWall 0 
      )
    else (false,(-1,-1),(-1,-1))
  )



let my_strategie (x,y) =
  (let (x0,y0) = (current_player()).start_position in (* begining of game *)
  if (x=x0) && (y=y0) then (
    cpt:= 0;
    marked_case := [])
  else ());


  let lst_ply = get_list_of_player () in
  let (b,p1,p2) = need_to_block lst_ply in (* test if i need to block *)
  if b then Placing_wall (p1,p2)
  else(
    cpt:= !cpt +1;
    match placeWallQuoridor () with (* else i make my quoridor and then i try to finish *)
    |None -> 
        let (x',y') = can_move (x,y) in
        pastx := x;pasty:=y;
        Moving(x',y')
    |Some (p1,p2) ->
      Placing_wall (p1,p2))
