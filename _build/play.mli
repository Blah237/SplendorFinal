open Card
open Ai

(**[init_state num_human num_ai] initilizes the first state and sets up the board*)
val init_state : int -> int -> state

(**[buy card p state c] returns the state [s] after [p] buys card [c]*)
val buy_card : player -> state -> card -> state option

(**[take_three_gems p state g1 g2 g3] returns the state after [p] takes three gems, or
* less if taking three gems is not possible *)
val take_three_gems : player -> state -> color -> color option -> color option -> state option

(** [take_two_gems p state gem] returns the state after [p] takes two gems,
* or [None] if the move is illegal *)
val take_two_gems : player -> state -> color -> state option

(** [reserve_card p state c] returns the state after [p] reserves [c] *)
val reserve_card : player -> state -> card -> state option

(** [reserve_top p state tier returns the state after [p] reserves the top card of
* the deck with tier [tier] *)
val reserve_top : player -> state -> int -> state option

(** [check_nobles p state nobles] returns a list of nobles that player [p] is eligible
* to take, or [None] if that player cannot take any*)
val check_nobles : player -> state -> noble list -> noble list option

(** [end game p state turns] ends the game when player [p] reaches 15 points, with
* [turns] turns remaining*)
val end_game : player -> state -> int -> state

(** [play s m] is the main function that plays the game, and returns the state
* after the current player makes move [m] *)
val play : state -> move -> state
