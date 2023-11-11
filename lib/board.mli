(** Module Board : Gestion du plateau de jeu Quoridor *)

open Types

(** {1 Types} *)

type cell_content
(** Type représentant le contenu d'une cellule sur le plateau. *)

type board
(** Type représentant le plateau de jeu. *)

(** {1 Constantes} *)

val board_size : int
(** Taille standard du plateau de jeu Quoridor. *)

(** {1 Fonctions} *)

val is_valid_position : int * int -> bool
(** Vérifie si une position est valide sur le plateau.
    @param pos Position à vérifier (x, y).
    @return [true] si la position est valide, [false] sinon. *)

val is_wall_position : int * int -> bool
(** Vérifie si la position donnée peut accueillir un mur.
    @param pos Position à vérifier (x, y).
    @return [true] si la position peut accueillir un mur, [false] sinon. *)

val is_player_position : int * int -> bool
(** Vérifie si la position donnée est celle d'un joueur.
    @param pos Position à vérifier (x, y).
    @return [true] si la position est celle d'un joueur, [false] sinon. *)

val is_wall : int * int -> bool
(** Vérifie si un mur se trouve à une position donnée.
    @param pos Position à vérifier (x, y).
    @return [true] si un mur est présent, [false] sinon. *)

val is_player : int * int -> bool
(** Vérifie si un joueur se trouve à une position donnée.
    @param pos Position à vérifier (x, y).
    @return [true] si un joueur est présent, [false] sinon. *)

val is_wall_between : int * int -> int * int -> bool
(** Vérifie s'il y a un mur entre deux positions adjacentes.
    @param pos1 Première position (x, y).
    @param pos2 Deuxième position (x, y).
    @return [true] si un mur est présent entre les deux positions, [false] sinon.
    @raise InvalidPosition si les positions ne sont pas adjacentes ou sont identiques. *)

val add_players_to_board : player list -> unit
(** Place les joueurs sur le plateau aux positions initiales.
    @param players Liste des joueurs à placer.
    @raise InvalidPlayerPosition si une position de joueur est invalide ou déjà occupée. *)

val update_player_position : player -> int * int -> unit
(** Déplace un joueur vers une nouvelle position.
    @param player Joueur à déplacer.
    @param pos Nouvelle position du joueur (x, y).
    @raise Invalid_argument si la nouvelle position est hors limites.
    @raise InvalidPlayerPosition si la nouvelle position est déjà occupée par un autre joueur. *)

val place_wall : int * int -> int * int -> unit
(** Place un mur entre deux positions valides.
    @param pos1 Première position du mur (x, y).
    @param pos2 Deuxième position du mur (x, y).
    @raise InvalidWallPosition si les positions pour le mur sont hors limites.
    @raise InvalidWallPlacement si un mur est déjà présent aux positions indiquées. *)

val print_board : unit -> unit
(** Affiche l'état actuel du plateau de jeu. *)
