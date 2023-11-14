type player
type cell_content
type board
type state

val board_size : int
val move_vectors : (int * int) list
val current_player : unit -> player
val update_player_order : unit -> unit
val pos_current_player : unit -> Types.position
val walls_left_current_player : unit -> int
val validate_position : Types.position -> unit
val is_wall_position : Types.position -> bool
val is_player_position : Types.position -> bool
val is_wall : Types.position -> bool
val is_player : Types.position -> bool
val is_wall_between : Types.position -> Types.position -> bool
val list_of_walls : Types.position -> (int * int) list
val list_of_players : Types.position -> (int * int) list
val list_of_moves : Types.position -> Types.position list
val validate_wall_placement : player -> Types.position -> Types.position -> unit
val place_wall : Types.position -> Types.position -> unit
val move_player : Types.position -> unit
val add_player_to_board : Types.color -> Types.position -> unit
val winning_player : unit -> player
val print_board : unit -> unit
