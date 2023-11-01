val init_board : Types.cell_content array array
val pp_board :   unit
val change_pos_of_player : Types.game -> Types.position -> Types.game
val move : Types.game -> Types.player -> Types.game
val place_wall_random : Types.game -> Types.player -> Types.game
val det_move : Types.game -> Types.player -> Types.game
