open Card

(** [determine move s] returns the move that the ai should make this turn*)
val determine_move : state -> move

(** [take_three_gems_helper ai three] returns the changes to the ai player 
 * that are necessary after the ai takes three gems *)
val take_three_gems_helper : player -> move -> player

(** [determine_discard s] returns the color of gem that the ai should discard when
forced to*)
val determine_discard : state -> color
