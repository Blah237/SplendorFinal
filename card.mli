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

(** colors represent the card requirements in colored cards to obtain a noble *)
type noble = gems


(** a player stores all of the necessary information about what a player has
 * done throughout the game*)
type player = {
	gems_held : gems;
		(** The number of gems a player has*) 
	discounts : gems;
		(** The number of discounts in each color a player has *)
	reserved : card list;
		(** The cards that the player has reserved (limit length 3) *)
	bought : int;
		(** the number of cards the player has bought, used for a tiebreaker at the 
		end of a game *)
	points : int;
		(** The number of points the player has *)
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

(** A state stores all of the information required in a game of Splendor and 
* is constantly updated as the game progresses *)
type state = {
	players : player list;
		(** a list of the players playing the game *)
	current_player : player;
		(** the player whose current turn it is *)
	tier1_deck : card list;
	tier2_deck : card list;
	tier3_deck : card list;
		(** the cards remaining in the decks of cards, seperated by tier *)
	tier1 : card list;
	tier2 : card list;
	tier3 : card list;
		(** the cards available, seperated by tier *)
	nobles : noble list;
		(** the nobles currently available *)
	available_gems : gems;
	gem_piles : int;
	(** used for checking how many gems a player can/must take if there are 
	less than three piles *)
}

