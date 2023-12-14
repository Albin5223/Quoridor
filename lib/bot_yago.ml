open Board
open Types

module BotYago = struct
  type direction = N | E | S | W

  let get_ending_direction = function
    | x, _ when x = 0 ->
        (* Start at left border *)
        E
    | x, _ when x = board_size - 1 ->
        (* Start at right border *)
        W
    | _, y when y = 0 ->
        (* Start at top border *)
        S
    | _, y when y = board_size - 1 ->
        (* Start at bottom border *)
        N
    | _ -> failwith "Invalid initial position"

  let is_ending_position (x, y) direction =
    match direction with
    | N -> y = 0
    | E -> x = board_size - 1
    | S -> y = board_size - 1
    | W -> x = 0

  let shortest_path_bfs start_point direction =
    let queue = Queue.create () in
    let rec bfs visited =
      if Queue.is_empty queue then None
      else
        let current_point, path = Queue.pop queue in
        if is_ending_position current_point direction then Some (List.rev path)
        else
          let moves = list_of_moves current_point in
          List.iter
            (fun move ->
              if not (List.mem move visited) then
                Queue.push (move, move :: path) queue)
            moves;
          bfs (current_point :: visited)
    in
    Queue.push (start_point, [ start_point ]) queue;
    bfs []

  let cases =
    List.init board_size (fun x -> List.init board_size (fun y -> (x, y)))
    |> List.flatten

  let valid_vertical_wall_positions =
    List.filter_map
      (fun (x, y) ->
        try
          validate_wall_placement 1 (x, y) (x, y + 1);
          Some ((x, y), (x, y + 1))
        with _ -> None)
      cases

  let pos_wall_random () =
    let valid_positions = valid_vertical_wall_positions in
    let random_index = Random.int (List.length valid_positions) in
    List.nth valid_positions random_index

  let player_number () = get_all_player_pos () |> List.length
  let is_in_middle_vertical_line (x, _) = x = board_size / 2

  let is_middle_line_free_of_walls () =
    List.init board_size (fun y -> (board_size / 2, y))
    |> List.for_all (fun pos -> not (is_wall pos))

  let in_game_placed_walls () =
    let total_walls =
      List.init board_size (fun x -> List.init board_size (fun y -> (x, y)))
      |> List.flatten
      |> List.filter (fun pos -> is_wall pos)
      |> List.length
    in
    total_walls / 2

  let should_place_wall_2_players current_player =
    let walls_placed_current_player = 10 - current_player.walls_left in
    let walls_placed_opponent =
      in_game_placed_walls () - walls_placed_current_player
    in
    let _, opponent_position =
      get_all_player_pos ()
      |> List.filter (fun (start, _) -> current_player.start_position <> start)
      |> List.hd
    in
    if
      is_in_middle_vertical_line opponent_position
      && is_in_middle_vertical_line current_player.current_position
      && is_middle_line_free_of_walls ()
    then
      if
        current_player.start_position = (board_size / 2, 0)
        && snd current_player.current_position <= 6
        && walls_placed_opponent mod 2 = 0
        && walls_placed_current_player mod 2 = 0
      then Some (pos_wall_random ())
      else None
    else None

  let should_place_wall () =
    let current_player = current_player () in
    if current_player.walls_left = 0 then None
    else
      let player_number = player_number () in
      if player_number = 2 then should_place_wall_2_players current_player
      else None

  let bot_yago pos =
    match should_place_wall () with
    | Some (pos1, pos2) -> Placing_wall (pos1, pos2)
    | None ->
        current_player ()
        |> (fun p -> p.start_position)
        |> get_ending_direction |> shortest_path_bfs pos |> Option.get
        |> List.tl |> List.hd
        |> fun x -> Moving x
end

let bot_yago pos = BotYago.bot_yago pos
