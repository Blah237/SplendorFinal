open Card

(*Initilizes the first state, setting the first player and setting up the board*)
val init_state : player -> state

(*[buy card p] returns the state after [p] buys a card*)
val buy_card : player -> card -> state

(*[take_three_gems p g1 g2 g3] returns the state after [p] takes three gems, or
* less if taking three gems is not possible *)
val take_three_gems : player -> color -> color option -> color option -> state

(** [take_two_gems p gem] returns the state after [p] takes two gems,
* or [Npne] if the move is illegal *)
val take_two_gems : player -> color -> state option

(** [reserve_card p c] returns the state after [p] buys [c] *)
val reserve_card : player -> card -> state

(** [check_nobles p nobles] returns a list of nobles that a player is eligible
* to take, or [None] if the player cannot take any*)
val check_nobles : player -> noble list -> noble list option

(** [end game p turns ends the game when player [p] reaches 15 points, with [turns]
* turns remaining*)
val end_game : player -> int -> state

(** [play s m] is the main function that plays the game, and returns the state 
* after the current player makes move [m] *)
val play : state -> move -> state
