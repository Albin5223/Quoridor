(** Module Types : Définitions des types de base pour le jeu Quoridor *)

(** {1 Types de Base} *)

(** Couleurs possibles pour les joueurs. *)
type color = Red | Green | Blue | Yellow

type position = int * int
(** Position sur le plateau, représentée par un tuple (x, y). *)

(** {1 Types de Joueurs et de Jeu} *)

type player = { position : position; walls_left : int; color : color }
(** Représente un joueur avec sa position, le nombre de murs restants et sa couleur. *)

(** État du jeu, soit en cours soit terminé avec le joueur gagnant. *)
type state = Ingame | GameOver of player

type game = {
  players : player list;
  current_player : player;
  state : state;
  winner : player option;
}
(** Structure globale du jeu incluant les joueurs, le joueur actuel, l'état du jeu et le gagnant. *)

(** {1 Exceptions} *)

exception OutOfBounds of string
(** Exceptions personnalisées pour la gestion d'erreurs spécifiques dans le jeu. *)

exception InvalidWallPosition of string
exception InvalidPlayerPosition of string
exception InvalidMove of string
exception InvalidPosition of string
exception InvalidWallPlacement of string
exception InvalidNumberPlayer of string
exception NoWinningPlayer of string
