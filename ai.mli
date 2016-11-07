type move = 
| Three of color * color option * color option
| Two of color
| Buy of card
| Reserve of card


(** [determine move s returns the move that the ai should make this turn*)
val determine_move : state -> move