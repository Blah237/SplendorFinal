type color =
| Red
| Blue
| Black
| Green
| White

type gems = {
	red : int;
	blue : int;
	black : int;
	green : int;
	white : int;
}

type card = {
	color : color;
		(** color of the card for discounts and nobles *)
	points : int;
		(** number of victory points the card is worth *)
	gem_cost : gems;
		(** cost in gems for the card of their respective colors *)
}

type player_type =
| Ai of color list
| Human

(** a player stores all of the necessary information about what a player has
 * done throughout the game*)
type player = {
 	name : string;
	gems_held : gems;
		(** The number of gems a player has*)
	discounts : gems;
		(** The number of discounts in each color a player has *)
	reserved : card list;
		(** The cards that the player has reserved (limit length 3) *)
	bought : int;
		(** the number of cards the player has bought, used for a tiebreaker
		 * at the end of a game *)
	points : int;
		(** The number of points the player has *)
	player_type : player_type;
		(** indicates whether the player is an ai or a human *)
	gold : int;
	  (* number of gold coins held by the player *)
}

(** the possible moves a player can make *)
type move =
| Three of color * color option * color option
	(** take three gems *)
| Two of color
	(** take two gems *)
| Buy of card
	(** buy a card *)
| Reserve of card
	(** reserve a card *)
| Top of int
	(** reserve the top card of a certain deck tier *)
| Pass 
	(** used by ai to pass, since taking gold is too complicated to handle *)

(** A state stores all of the information required in a game of Splendor and
* is constantly updated as the game progresses *)
type state = {
	players : player list;
		(** a list of the players playing the game *)
	tier1_deck : card list;
	tier2_deck : card list;
	tier3_deck : card list;
		(** the cards remaining in the decks of cards, seperated by tier *)
	tier1 : card list;
	tier2 : card list;
	tier3 : card list;
		(** the cards available, seperated by tier *)
	nobles : gems list;
		(** the nobles currently available *)
	available_gems : gems;
		(** the gems currently available for taking *)
	gem_piles : int;
	(** used for checking how many gems a player can/must take if there are
	less than three piles *)
	turns_taken : int;
	(** used for ai behavior *)
	gold : int;
	(** the number of gold coins available **)
}
