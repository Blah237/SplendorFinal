open Card

(** [determine move s] returns the move that the ai should make this turn*)
val determine_move : state -> move

(** [determine_discard s] returns the color of gem that the ai should discard when
forced to*)
val determine_discard : state -> color
