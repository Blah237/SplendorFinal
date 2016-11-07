(*Initilizes the first state, setting the first player and setting up the board*)
val init_state : player -> state

(*[buy card p] returns the state after [p] buys a card*)
val buy_card : player -> card -> state

(*[take_three_gems p g1 g2 g3] returns the state after [p] takes three gems*)
val take_three_gems : player -> color -> color option -> color option -> state

(** [take_two_gems p g1 g2] returns the state after [p] takes two gems,
* or [Npne] if the move is illegal *)
val take_two_gems : player -> color -> color -> state option

(** [reserve_card p c] returns the state after [p] buys [c] *)
val reserve_card : player -> card -> state

(** [end game p turns ends the game when player [p] reaches 15 points, with [turns]
* turns remaining*)
val end_game : player -> int -> unit

