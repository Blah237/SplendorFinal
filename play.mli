open Card
open Ai
open Graphic

(**[init_state starting_player] initilizes the first state, setting 
the first player to [starting_player] and setting up the board*)
val init_state : player -> state

(**[buy card p c] returns the state after [p] buys card [c]*)
val buy_card : player -> card -> state

(**[take_three_gems p g1 g2 g3] returns the state after [p] takes three gems, or
* less if taking three gems is not possible *)
val take_three_gems : player -> color -> color option -> color option -> state

(** [take_two_gems p gem] returns the state after [p] takes two gems,
* or [None] if the move is illegal *)
val take_two_gems : player -> color -> state option

(** [reserve_card p c] returns the state after [p] reserves [c] *)
val reserve_card : player -> card -> state

(** [reserve_top p tier returns the state after [p] reserves the top card of
* the deck with tier [tier] *)
val reserve_top : player -> int -> state

(** [check_nobles p nobles] returns a list of nobles that player [p] is eligible
* to take, or [None] if that player cannot take any*)
val check_nobles : player -> noble list -> noble list option

(** [end game p turns] ends the game when player [p] reaches 15 points, with
* [turns] turns remaining*)
val end_game : player -> int -> state

(** [play s m] is the main function that plays the game, and returns the state 
* after the current player makes move [m] *)
val play : state -> move -> state
