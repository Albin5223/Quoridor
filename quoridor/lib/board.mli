val board_size : int
val is_valid_position : int * int -> bool
val get_cell_content : int * int -> 'a array array -> 'a
val is_wall : Types.cell_content -> bool
val is_player : Types.cell_content -> bool
val is_wall_position : int * int -> bool
val is_player_position : int * int -> bool
val move_vectors : (int * int) array
val list_of_walls :
  int * int -> Types.cell_content array array -> (int * int) array
val list_of_players :
  int * int -> Types.cell_content array array -> (int * int) array
val is_wall_between :
  int * int -> int * int -> Types.cell_content array array -> bool
val list_of_moves :
  int * int -> Types.cell_content array array -> (int * int) array
val dfs_path_exists : int * int -> Types.cell_content array array -> bool
val can_place_wall :
  int * int -> (int * int) array -> Types.cell_content array array -> bool
val place_wall :
  int * int ->
  (int * int) array ->
  Types.cell_content array array -> Types.cell_content array array