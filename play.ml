(*open Card
open Ai*)

let empty_gems = {red = 0; blue = 0; black = 0; green = 0; white = 0}

let make_card color points gem_cost =
	{color = color;
	points = points;
	gem_cost = gem_cost}

let make_tier_1 =
	let t1_1 = make_card White 0
	{red = 0;
	blue = 3;
	black = 0;
	green = 0;
	white = 0} in
	let t1_2 = make_card White 0
	{red = 2;
	blue = 0;
	black = 1;
	green = 0;
	white = 0} in
	let t1_3 = make_card White 0
	{red = 1;
	blue = 1;
	black = 1;
	green = 1;
	white = 0} in
	let t1_4 = make_card White 0
	{red = 0;
	blue = 2;
	black = 2;
	green = 0;
	white = 0} in
	let t1_5 = make_card White 1
	{red = 0;
	blue = 0;
	black = 0;
	green = 4;
	white = 0} in
	let t1_6 = make_card White 0
	{red = 1;
	blue = 1;
	black = 1;
	green = 2;
	white = 0} in
	let t1_7 = make_card White 0
	{red = 0;
	blue = 2;
	black = 1;
	green = 2;
	white = 0} in
	let t1_8 = make_card White 0
	{red = 0;
	blue = 1;
	black = 1;
	green = 0;
	white = 3} in
	let t1_9 = make_card Blue 0
	{red = 0;
	blue = 0;
	black = 2;
	green = 0;
	white = 1} in
	let t1_10 = make_card Blue 0
	{red = 0;
	blue = 0;
	black = 3;
	green = 0;
	white = 0} in
	let t1_11 = make_card Blue 0
	{red = 1;
	blue = 0;
	black = 1;
	green = 1;
	white = 1} in
	let t1_12 = make_card Blue 0
	{red = 0;
	blue = 0;
	black = 2;
	green = 2;
	white = 0} in
	let t1_13 = make_card Blue 1
	{red = 4;
	blue = 0;
	black = 0;
	green = 0;
	white = 0} in
	let t1_14 = make_card Blue 0
	{red = 2;
	blue = 0;
	black = 1;
	green = 1;
	white = 1} in
	let t1_15 = make_card Blue 0
	{red = 2;
	blue = 0;
	black = 0;
	green = 2;
	white = 1} in
	let t1_16 = make_card Blue 0
	{red = 1;
	blue = 1;
	black = 0;
	green = 3;
	white = 0} in
	let t1_17 = make_card Green 0
	{red = 0;
	blue = 1;
	black = 0;
	green = 0;
	white = 2} in
	let t1_18 = make_card Green 0
	{red = 3;
	blue = 0;
	black = 0;
	green = 0;
	white = 0} in
	let t1_19 = make_card Green 0
	{red = 1;
	blue = 1;
	black = 1;
	green = 0;
	white = 1} in
	let t1_20 = make_card Green 0
	{red = 2;
	blue = 2;
	black = 0;
	green = 0;
	white = 0} in
	let t1_21 = make_card Green 1
	{red = 0;
	blue = 0;
	black = 4;
	green = 0;
	white = 0} in
	let t1_22 = make_card Green 0
	{red = 1;
	blue = 1;
	black = 2;
	green = 0;
	white = 1} in
	let t1_23 = make_card Green 0
	{red = 2;
	blue = 1;
	black = 2;
	green = 0;
	white = 0} in
	let t1_24 = make_card Green 0
	{red = 0;
	blue = 3;
	black = 0;
	green = 1;
	white = 1} in
	let t1_25 = make_card Red 0
	{red = 0;
	blue = 2;
	black = 0;
	green = 1;
	white = 0} in
	let t1_26 = make_card Red 0
	{red = 0;
	blue = 0;
	black = 0;
	green = 0;
	white = 3} in
	let t1_27 = make_card Red 0
	{red = 0;
	blue = 1;
	black = 1;
	green = 1;
	white = 1} in
	let t1_28 = make_card Red 0
	{red = 2;
	blue = 0;
	black = 0;
	green = 0;
	white = 2} in
	let t1_29 = make_card Red 1
	{red = 0;
	blue = 0;
	black = 0;
	green = 0;
	white = 4} in
	let t1_30 = make_card Red 0
	{red = 0;
	blue = 1;
	black = 1;
	green = 1;
	white = 2} in
	let t1_31 = make_card Red 0
	{red = 0;
	blue = 0;
	black = 2;
	green = 1;
	white = 2} in
	let t1_32 = make_card Red 0
	{red = 1;
	blue = 0;
	black = 3;
	green = 0;
	white = 1} in
	let t1_33 = make_card Black 0
	{red = 1;
	blue = 0;
	black = 0;
	green = 2;
	white = 0} in
	let t1_34 = make_card Black 0
	{red = 0;
	blue = 0;
	black = 0;
	green = 3;
	white = 0} in
	let t1_35 = make_card Black 0
	{red = 1;
	blue = 1;
	black = 0;
	green = 1;
	white = 1} in
	let t1_36 = make_card Black 0
	{red = 0;
	blue = 0;
	black = 0;
	green = 2;
	white = 2} in
	let t1_37 = make_card Black 1
	{red = 0;
	blue = 4;
	black = 0;
	green = 0;
	white = 0} in
	let t1_38 = make_card Black 0
	{red = 1;
	blue = 2;
	black = 0;
	green = 1;
	white = 1} in
	let t1_39 = make_card Black 0
	{red = 1;
	blue = 2;
	black = 0;
	green = 0;
	white = 2} in
	let t1_40 = make_card Black 0
	{red = 3;
	blue = 0;
	black = 1;
	green = 1;
	white = 0} in
  [t1_1;t1_2;t1_3;t1_4;t1_5;t1_6;t1_7;t1_8;t1_9;t1_10;
	t1_11;t1_12;t1_13;t1_14;t1_15;t1_16;t1_17;t1_18;t1_19;t1_20;
	t1_21;t1_22;t1_23;t1_24;t1_25;t1_26;t1_27;t1_28;t1_29;t1_30;
	t1_31;t1_32;t1_33;t1_34;t1_35;t1_36;t1_37;t1_38;t1_39;t1_40]

	let make_tier_2 =
		let t1_1 = make_card White 2
		{red = 5;
		blue = 0;
		black = 0;
		green = 0;
		white = 0} in
		let t1_2 = make_card White 3
		{red = 0;
		blue = 0;
		black = 0;
		green = 0;
		white = 6} in
		let t1_3 = make_card White 1
		{red = 2;
		blue = 0;
		black = 2;
		green = 3;
		white = 0} in
		let t1_4 = make_card White 2
		{red = 4;
		blue = 0;
		black = 2;
		green = 1;
		white = 0} in
		let t1_5 = make_card White 1
		{red = 3;
		blue = 3;
		black = 0;
		green = 0;
		white = 2} in
		let t1_6 = make_card White 2
		{red = 5;
		blue = 0;
		black = 3;
		green = 0;
		white = 0} in




		let t1_9 = make_card Blue 2
		{red = 0;
		blue = 0;
		black = 0;
		green = 5;
		white = 0} in
		let t1_10 = make_card Blue 3
		{red = 0;
		blue = 0;
		black = 0;
		green = 6;
		white = 0} in
		let t1_11 = make_card Blue 1
		{red = 0;
		blue = 3;
		black = 2;
		green = 0;
		white = 2} in
		let t1_12 = make_card Blue 1
		{red = 3;
		blue = 0;
		black = 0;
		green = 2;
		white = 3} in
		let t1_13 = make_card Blue 2
		{red = 0;
		blue = 2;
		black = 1;
		green = 0;
		white = 4} in
		let t1_14 = make_card Blue 2
		{red = 0;
		blue = 5;
		black = 0;
		green = 3;
		white = 0} in



		let t1_17 = make_card Green 2
		{red = 0;
		blue = 0;
		black = 0;
		green = 5;
		white = 0} in
		let t1_18 = make_card Green 3
		{red = 0;
		blue = 0;
		black = 0;
		green = 6;
		white = 0} in
		let t1_19 = make_card Green 1
		{red = 0;
		blue = 3;
		black = 2;
		green = 0;
		white = 2} in
		let t1_20 = make_card Green 1
		{red = 3;
		blue = 0;
		black = 0;
		green = 2;
		white = 3} in
		let t1_21 = make_card Green 2
		{red = 0;
		blue = 2;
		black = 1;
		green = 0;
		white = 4} in
		let t1_22 = make_card Green 2
		{red = 0;
		blue = 5;
		black = 0;
		green = 3;
		white = 0} in



		let t1_25 = make_card Red 2
		{red = 0;
		blue = 0;
		black = 5;
		green = 0;
		white = 0} in
		let t1_26 = make_card Red 3
		{red = 6;
		blue = 0;
		black = 0;
		green = 0;
		white = 0} in
		let t1_27 = make_card Red 1
		{red = 2;
		blue = 0;
		black = 3;
		green = 0;
		white = 2} in
		let t1_28 = make_card Red 2
		{red = 0;
		blue = 4;
		black = 0;
		green = 2;
		white = 1} in
		let t1_29 = make_card Red 1
		{red = 2;
		blue = 3;
		black = 3;
		green = 0;
		white = 0} in
		let t1_30 = make_card Red 2
		{red = 0;
		blue = 0;
		black = 5;
		green = 0;
		white = 3} in



		let t1_33 = make_card Black 2
		{red = 0;
		blue = 0;
		black = 5;
		green = 0;
		white = 0} in
		let t1_34 = make_card Black 3
		{red = 0;
		blue = 0;
		black = 6;
		green = 0;
		white = 0} in
		let t1_35 = make_card Black 1
		{red = 0;
		blue = 2;
		black = 0;
		green = 2;
		white = 3} in
		let t1_36 = make_card Black 2
		{red = 2;
		blue = 1;
		black = 0;
		green = 4;
		white = 0} in
		let t1_37 = make_card Black 1
		{red = 0;
		blue = 0;
		black = 2;
		green = 3;
		white = 3} in
		let t1_38 = make_card Black 2
		{red = 3;
		blue = 0;
		black = 0;
		green = 5;
		white = 0} in

	  [t1_1;t1_2;t1_3;t1_4;t1_5;t1_6;
		 t1_9;t1_10;t1_11;t1_12;t1_13;t1_14;
		 t1_17;t1_18;t1_19;t1_20;t1_21;t1_22;
		 t1_25;t1_26;t1_27;t1_28;t1_29;t1_30;
	   t1_33;t1_34;t1_35;t1_36;t1_37;t1_38]



 let make_tier_3 =
		let t1_1 = make_card White 4
		{red = 0;
		blue = 0;
		black = 7;
		green = 0;
		white = 0} in
		let t1_2 = make_card White 5
		{red = 0;
		blue = 0;
		black = 7;
		green = 0;
		white = 3} in
		let t1_3 = make_card White 4
		{red = 3;
		blue = 0;
		black = 6;
		green = 0;
		white = 3} in
		let t1_4 = make_card White 3
		{red = 5;
		blue = 3;
		black = 3;
		green = 3;
		white = 0} in



		let t1_9 = make_card Blue 4
		{red = 0;
		blue = 0;
		black = 0;
		green = 0;
		white = 7} in
		let t1_10 = make_card Blue 5
		{red = 0;
		blue = 3;
		black = 0;
		green = 0;
		white = 7} in
		let t1_11 = make_card Blue 4
		{red = 0;
		blue = 3;
		black = 3;
		green = 0;
		white = 6} in
		let t1_12 = make_card Blue 3
		{red = 3;
		blue = 0;
		black = 5;
		green = 3;
		white = 3} in




		let t1_17 = make_card Green 4
		{red = 0;
		blue = 7;
		black = 0;
		green = 0;
		white = 0} in
		let t1_18 = make_card Green 5
		{red = 0;
		blue = 7;
		black = 0;
		green = 3;
		white = 0} in
		let t1_19 = make_card Green 4
		{red = 0;
		blue = 6;
		black = 0;
		green = 3;
		white = 3} in
		let t1_20 = make_card Green 3
		{red = 3;
		blue = 3;
		black = 3;
		green = 0;
		white = 5} in




		let t1_25 = make_card Red 4
		{red = 0;
		blue = 0;
		black = 0;
		green = 7;
		white = 0} in
		let t1_26 = make_card Red 5
		{red = 3;
		blue = 0;
		black = 0;
		green = 7;
		white = 0} in
		let t1_27 = make_card Red 4
		{red = 3;
		blue = 3;
		black = 0;
		green = 6;
		white = 0} in
		let t1_28 = make_card Red 3
		{red = 0;
		blue = 5;
		black = 3;
		green = 3;
		white = 3} in




		let t1_33 = make_card Black 4
		{red = 7;
		blue = 0;
		black = 0;
		green = 0;
		white = 0} in
		let t1_34 = make_card Black 5
		{red = 7;
		blue = 0;
		black = 3;
		green = 0;
		white = 0} in
		let t1_35 = make_card Black 4
		{red = 3;
		blue = 3;
		black = 6;
		green = 0;
		white = 0} in
		let t1_36 = make_card Black 3
		{red = 3;
		blue = 3;
		black = 0;
		green = 5;
		white = 3} in


	  [t1_1;t1_2;t1_3;t1_4;
		 t1_9;t1_10;t1_11;t1_12;
		 t1_17;t1_18;t1_19;t1_20;
		 t1_25;t1_26;t1_27;t1_28;
	   t1_33;t1_34;t1_35;t1_36;]


let make_nobles =
	let t1_1 =
	{red = 0;
	blue = 3;
	black = 3;
	green = 0;
	white = 3} in
	let t1_2 =
	{red = 3;
	blue = 3;
	black = 0;
	green = 3;
	white = 0} in
	let t1_3 =
	{red = 3;
	blue = 0;
	black = 3;
	green = 0;
	white = 3} in
	let t1_4 =
	{red = 4;
	blue = 0;
	black = 0;
	green = 4;
	white = 0} in
	let t1_5 =
	{red = 0;
	blue = 4;
	black = 0;
	green = 4;
	white = 0} in
	let t1_6 =
	{red = 4;
	blue = 0;
	black = 4;
	green = 0;
	white = 0} in
	let t1_7 =
	{red = 0;
	blue = 0;
	black = 4;
	green = 0;
	white = 4} in
	let t1_8 =
	{red = 0;
	blue = 3;
	black = 0;
	green = 3;
	white = 3} in
	let t1_9 =
	{red = 3;
	blue = 0;
	black = 3;
	green = 3;
	white = 0} in
	let t1_10 =
	{red = 0;
	blue = 4;
	black = 0;
	green = 0;
	white = 4} in
	[t1_1;t1_2;t1_3;t1_4;t1_5;t1_6;t1_7;t1_8;t1_9;t1_10;]


(* Discards a single gem [gem] from the players hand [p] *)
(* Returns a players gems_held after discard 1 gem *)
(* Needs to check if they have atleast 1 gem of that kind *)
let discard_gem player gem =
match gem with
| Red ->
	{
		red = player.gems_held.red -1;
		blue = player.gems_held.blue;
		black = player.gems_held.black;
		green = player.gems_held.green;
		white = player.gems_held.white;
	}
| Blue ->
	{
		red = player.gems_held.red;
		blue = player.gems_held.blue-1;
		black = player.gems_held.black;
		green = player.gems_held.green;
		white = player.gems_held.white;
	}
| Green ->
	{
		red = player.gems_held.red;
		blue = player.gems_held.blue;
		black = player.gems_held.black;
		green = player.gems_held.green-1;
		white = player.gems_held.white;
	}
| White ->
	{
		red = player.gems_held.red;
		blue = player.gems_held.blue;
		black = player.gems_held.black;
		green = player.gems_held.green;
		white = player.gems_held.white-1;
	}
| Black ->
	{
		red = player.gems_held.red;
		blue = player.gems_held.blue;
		black = player.gems_held.black-1;
		green = player.gems_held.green;
		white = player.gems_held.white;
	}

(* Calculates distance between a card and players held gems and discounts *)
let calc_dis p c =
	let r = c.gem_cost.red - p.gems_held.red - p.discounts.red in
	let g = c.gem_cost.green - p.gems_held.green - p.discounts.green in
	let w = c.gem_cost.white - p.gems_held.white - p.discounts.white in
	let b = c.gem_cost.blue - p.gems_held.blue - p.discounts.blue in
	let bl = c.gem_cost.black - p.gems_held.black - p.discounts.black in
	let map = [r;g;w;b;bl] in
    List.fold_left (fun d x -> if x > 0 then d+x else d) 0 map


(* True if player p can purchase card c, false otherwise *)
(* Includes gold and whether they need to use it as the snd val *)
let can_buy p c =
	if calc_dis p c <= p.gold then (true,calc_dis p c)
	else (false,0)


(* Creates a new discount gem list based on current and card *)
let make_new_discounts p c =
match c.color with
| Red ->
	{
	 red = p.red + 1;
	 blue = p.blue;
	 black = p.black;
	 green = p.green;
	 white = p.white;
	}
| Blue ->
	{
		red = p.red;
		blue = p.blue + 1;
		black = p.black;
		green = p.green;
		white = p.white;
	}
| White ->
	{
		red = p.red;
		blue = p.blue;
		black = p.black;
		green = p.green;
		white = p.white + 1;
	}
| Green ->
	{
		red = p.red;
		blue = p.blue;
		black = p.black;
		green = p.green +1;
		white = p.white;
	}
| Black ->
	{
		red = p.red;
		blue = p.blue;
		black = p.black + 1;
		green = p.green;
		white = p.white;
	}

(* Calculates the num of gems a player has *)
let calc_players_gems gems =
	let r = gems.red in
	let g = gems.green in
	let w = gems.white in
	let b = gems.blue in
	let bl = gems.black in
	let map = [r;g;w;b;bl] in
	List.fold_left (fun d x ->if x > 0 then d+x else d) 0 map

(* Calculates the num of gem piles*)
let calc_gem_piles gems =
	let r = gems.red in
	let g = gems.green in
	let w = gems.white in
	let b = gems.blue in
	let bl = gems.black in
	let map = [r;g;w;b;bl] in
	List.fold_left (fun d x ->if x > 0 then d+1 else d) 0 map

(* Check if an item exists in a list *)
let exists k l =
    List.fold_left (fun b x -> b || x = k) false l


(* replaces the old player [p] with new player [p2] and returns the list*)
let rec find_p p p2 lst=
match lst with
| [] -> []
| h::t ->
   if h = p then p2::find_p p p2 t
	 else h::find_p p p2 t

(* removes [p] and returns the list*)
let rec remove p lst=
match lst with
| [] -> []
| h::t ->
   if h = p then t
	 else h::(remove p t)

(* Takes a tier deck, cards out for that tier and returns
 * a new list of cards out for that tier popped form the top of the deck
 * also removes the card from the field *)
 let refresh_cards tier_deck tier_out c =
 match tier_deck with
 | [] -> ([],remove c tier_out)
 | h::t ->
  (* pops the first card, replaces it with the bought card *)
  (t, find_p c h tier_out)

(* returns a randomized version of [lst] *)
let shuffle lst =
    let rand = List.map (fun c -> (Random.float 1.0, c)) lst in
    let sort_rand = List.sort compare rand in
    List.map snd sort_rand

(* returns a tuple of the first four elements of the list and the rest of the list *)
let four_rest lst =
	match lst with
	| a::b::c::d::tl -> ([a;b;c;d],tl)
	| _ -> (lst,[])

(* creates a list of length [len] with all elements equal to [x] *)
let rec make_list len x =
	match len with
	| 0 -> []
	| _ -> x::(make_list (len - 1) x)

(* return the first [n] elements of a list [lst] *)
let rec felements n lst =
	if n = 0 then [] else
	match lst with
	| [] -> []
	| h::t -> h::(felements (n-1) t)

(* Returns a tuple of the updated player and the available gems on
the board after player [p] takes [n] gems of color [gem] when [s]
represents the number of gems available *)
let take_gem n gem p g =
	let tup =
	match gem with
	| Red ->
			(* modify player and state *)
				let new_state_gems = {
					red = g.red - n;
					blue = g.blue;
					black = g.black;
					green = g.green;
					white = g.white;
				} in
			  (* New gem pile *)
				let new_gems = {
					red = p.gems_held.red + n;
					blue = p.gems_held.blue;
					black = p.gems_held.black;
					green = p.gems_held.green;
					white = p.gems_held.white;
				} in
				(new_gems, new_state_gems)
	| Blue ->
				(* modify player and state *)
				let new_state_gems = {
					red = g.red;
					blue = g.blue - n;
					black = g.black;
					green = g.green;
					white = g.white;
				} in
			  (* New gem pile *)
				let new_gems = {
					red = p.gems_held.red;
					blue = p.gems_held.blue + n;
					black = p.gems_held.black;
					green = p.gems_held.green;
					white = p.gems_held.white;
				} in
				(new_gems, new_state_gems)
	| Black ->
				(* modify player and state *)
				let new_state_gems = {
					red = g.red;
					blue = g.blue;
					black = g.black - n;
					green = g.green;
					white = g.white;
				} in
			  (* New gem pile *)
				let new_gems = {
					red = p.gems_held.red;
					blue = p.gems_held.blue;
					black = p.gems_held.black + n;
					green = p.gems_held.green;
					white = p.gems_held.white;
				} in
				(new_gems, new_state_gems)
	| Green ->
				(* modify player and state *)
				let new_state_gems = {
					red = g.red;
					blue = g.blue;
					black = g.black;
					green = g.green - n;
					white = g.white;
				} in
			  (* New gem pile *)
				let new_gems = {
					red = p.gems_held.red;
					blue = p.gems_held.blue;
					black = p.gems_held.black;
					green = p.gems_held.green + n;
					white = p.gems_held.white;
				} in
				(new_gems, new_state_gems)
	| White ->
				(* modify player and state *)
				let new_state_gems = {
					red = g.red;
					blue = g.blue;
					black = g.black;
					green = g.green;
					white = g.white - n;
				} in
			  (* New gem pile *)
				let new_gems = {
					red = p.gems_held.red;
					blue = p.gems_held.blue;
					black = p.gems_held.black;
					green = p.gems_held.green;
					white = p.gems_held.white + n;
				} in
				(new_gems, new_state_gems)
	in
	(* Modified player *)
	let p_new = {
		gems_held = fst tup;
		discounts = p.discounts;
		reserved = p.reserved;
		bought = p.bought;
		gold = p.gold;
		points = p.points;
		player_type = p.player_type;
		} in
	(p_new, snd tup)


(* make the players, randomize the order, shuffle decks, putting out cards *)
let init_state num_human num_ai =
	let plyr_human = {gems_held = empty_gems;
				discounts = empty_gems;
				reserved = [];
				bought = 0;
				points = 0;
				gold = 0;
				player_type = Human} in
	let plyr_ai = {gems_held = empty_gems;
				discounts = empty_gems;
				reserved = [];
				bought = 0;
				points = 0;
				gold = 0;
				player_type = Ai []} in
	let human_players = make_list num_human plyr_human in
	let ai_players = make_list num_ai plyr_ai in
	let plyrs = human_players@ai_players in
	let tier_one = four_rest (shuffle make_tier_1) in
	let tier_two = four_rest (shuffle make_tier_2) in
	let tier_three = four_rest (shuffle make_tier_3) in
	let g =
	match num_human + num_ai with
	| 2 -> 4
	| 3 -> 5
	| _ -> 7
    in
    let starting_gems = {red = g; blue = g; black = g; green = g; white = g} in
	{players = shuffle plyrs;
		(** a list of the players playing the game *)
	tier1_deck = snd tier_one;
	tier2_deck = snd tier_two;
	tier3_deck = snd tier_three;
		(** the cards remaining in the decks of cards, seperated by tier *)
	tier1 = fst tier_one;
	tier2 = fst tier_two;
	tier3 = fst tier_three;
		(** the cards available, seperated by tier *)
	nobles = felements (num_human + num_ai + 1) (shuffle make_nobles);
		(** the nobles currently available *)
	available_gems = starting_gems;
		(** the gems currently available for taking *)
	gem_piles = 5;
	(** used for checking how many gems a player can/must take if there are
	less than three piles *)
	turns_taken = 0;
	(** used for ai behavior *)
	gold = 5;
	(** the number of gold coins available **)
}

let buy_card p s c =
  (* need to see which pile this card belongs to *)
  (* First Checking if they can afford the card *)
	match can_buy p c with
	| (true,g) ->
		let red_g = max 0 (min p.gems_held.red (c.gem_cost.red - p.discounts.red)) in
		let blue_g = max 0 (min p.gems_held.blue (c.gem_cost.blue - p.discounts.blue)) in
		let green_g = max 0 (min p.gems_held.green (c.gem_cost.green - p.discounts.green)) in
		let white_g = max 0 (min p.gems_held.white (c.gem_cost.white - p.discounts.white)) in
		let black_g = max 0 (min p.gems_held.black (c.gem_cost.black - p.discounts.black)) in
		 (* Update player's gems *)
		 let new_player_gems = {
			 red = p.gems_held.red - red_g;
			 blue = p.gems_held.blue - blue_g;
			 green = p.gems_held.green - green_g;
			 white = p.gems_held.white - white_g;
			 black = p.gems_held.black - black_g;
		 } in
		 (* Update gems on table *)
		 let table_gems = {
			 red = s.available_gems.red + red_g;
			 blue = s.available_gems.blue + blue_g;
			 green = s.available_gems.green + green_g;
			 white = s.available_gems.white + white_g;
			 black = s.available_gems.black + black_g;
		 } in
		 (* update discounts *)
		 let new_discounts = make_new_discounts p.discounts c in
		 let bought_cards = p.bought + 1 in
		 let pts = p.points + c.points in
		 let updated_player = {
			 gems_held = new_player_gems;
			 discounts = new_discounts;
			 points = pts;
			 bought = bought_cards;
			 gold = p.gold - g;
			 player_type = p.player_type;
			 reserved = p.reserved;
		 } in
		 (* Finished updated the player need to update state *)
		 if exists c s.tier1 then
		   let (deck,table) = refresh_cards s.tier1_deck s.tier1 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = table;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = table_gems;
					 (** the gems currently available for taking *)
				 gem_piles = calc_gem_piles s.available_gems;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = s.gold + g;
				 (** the number of gold coins available **)
			 }
		 else if exists c s.tier2 then
			 let (deck,table) = refresh_cards s.tier2_deck s.tier2 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = table;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = table_gems;
					 (** the gems currently available for taking *)
				 gem_piles = calc_gem_piles s.available_gems;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = s.gold + g;
				 (** the number of gold coins available **)
			 }
		 else if exists c s.tier3 then
			 let (deck,table) = refresh_cards s.tier3_deck s.tier3 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = table;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = table_gems;
					 (** the gems currently available for taking *)
				 gem_piles = calc_gem_piles s.available_gems;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = s.gold + g;
				 (** the number of gold coins available **)
			 }
		else (* Case of buying a reserved card, not from the board *)
			let new_player = {
			 gems_held = updated_player.gems_held;
			 discounts = updated_player.discounts;
			 points = updated_player.points;
			 bought = updated_player.bought;
			 gold = updated_player.gold;
			 player_type = p.player_type;
			 reserved = remove c p.reserved;
			 }
			in let new_p_list = List.tl s.players @ [new_player] in
			Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = table_gems;
					 (** the gems currently available for taking *)
				 gem_piles = calc_gem_piles s.available_gems;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = s.gold + g;
				 (** the number of gold coins available **)
			 }
	| _ -> None

(* returns the value inside an option *)
let get x =
	match x with
	| Some(y) -> y

let take_three_gems p s g1 g2 g3 =
(************** case of >10 gems *********************)
	let same_color =
	match (g1,g2,g3) with
	| (x,Some(y),Some(z)) -> if x = y || y = z || x = z then true else false
	| (x,None,Some(z)) -> if x = z then true else false
	| (x,Some(y),None) -> if x = y then true else false
	| _ -> false
	in if same_color then (s, "Illegal move. You cannot pick three gems if two or more are the same color.")
	else
	let tup =
	match g1 with
	| Red ->
	   (* if available gems < 4 then none *)
	   if s.available_gems.red < 1 then None
	   else Some (take_gem 1 Red p s.available_gems)
	| Blue ->
		if s.available_gems.blue < 1 then None
	    else Some (take_gem 1 Blue p s.available_gems)
	| Green ->
		if s.available_gems.green < 1 then None
	    else Some (take_gem 1 Green p s.available_gems)
	| Black ->
		if s.available_gems.black < 1 then None
	    else Some (take_gem 1 Black p s.available_gems)
	| White ->
		if s.available_gems.white < 1 then None
	    else Some (take_gem 1 White p s.available_gems)
	in
	if tup = None then (s, "Illegal move. One of the colors you selected has no gems available.")
	else
	let tup = get tup in
	let tup2 =
	match g2 with
	| Some(Red) ->
	   (* if available gems < 4 then none *)
	   if (snd tup).red < 1 then None
	   else Some (take_gem 1 Red (fst tup) (snd tup))
	| Some(Blue) ->
		if (snd tup).blue < 1 then None
	    else Some (take_gem 1 Blue (fst tup) (snd tup))
	| Some(Green) ->
		if (snd tup).green < 1 then None
	    else Some (take_gem 1 Green (fst tup) (snd tup))
	| Some(Black) ->
		if (snd tup).black < 1 then None
	    else Some (take_gem 1 Black (fst tup) (snd tup))
	| Some(White) ->
		if (snd tup).white < 1 then None
	    else Some (take_gem 1 White (fst tup) (snd tup))
	| None ->
		 Some tup
	in
	if tup2 = None then (s,"Illegal move. One of the colors you selected has no gems available.")
	else
	let tup2 = get tup2 in
	let tup3 =
	match g3 with
	| Some(Red) ->
	   (* if available gems < 4 then none *)
	   if (snd tup2).red < 1 then None
	   else Some (take_gem 1 Red (fst tup2) (snd tup2))
	| Some(Blue) ->
		if (snd tup2).blue < 1 then None
	    else Some (take_gem 1 Blue (fst tup2) (snd tup2))
	| Some(Green) ->
		if (snd tup2).green < 1 then None
	    else Some (take_gem 1 Green (fst tup2) (snd tup2))
	| Some(Black) ->
		if (snd tup2).black < 1 then None
	    else Some (take_gem 1 Black (fst tup2) (snd tup2))
	| Some(White) ->
		if (snd tup2).white < 1 then None
	    else Some (take_gem 1 White (fst tup2) (snd tup2))
	| None -> Some tup2
	in
	match tup3 with
	| Some(p, g) -> let new_state =
					{players = List.tl s.players @ [p];
						(** a list of the players playing the game *)
					tier1_deck = s.tier1_deck;
					tier2_deck = s.tier2_deck;
					tier3_deck = s.tier3_deck;
						(** the cards remaining in the decks of cards, seperated by tier *)
					tier1 = s.tier1;
					tier2 = s.tier2;
					tier3 = s.tier3;
						(** the cards available, seperated by tier *)
					nobles = s.nobles;
						(** the nobles currently available *)
					available_gems = g;
						(** the gems currently available for taking *)
					gem_piles = s.gem_piles;
					(** used for checking how many gems a player can/must take if there are
					less than three piles *)
					turns_taken = s.turns_taken + 1;
					(** used for ai behavior *)
					gold = s.gold;
					(** the number of gold coins available **)
				}
			in (new_state, "")
	| None -> (s, "Illegal move. One of the colors you selected has no gems available.")

let take_two_gems p s gem =
(**************** Needs to check if player has > 10 gems ********************)
	let tup =
	match gem with
	| Red ->
	   (* if available gems < 4 then none *)
	   if s.available_gems.red < 4 then None
	   else Some (take_gem 2 Red p s.available_gems)
	| Blue ->
		if s.available_gems.blue < 4 then None
	    else Some (take_gem 2 Blue p s.available_gems)
	| Green ->
		if s.available_gems.green < 4 then None
	    else Some (take_gem 2 Green p s.available_gems)
	| Black ->
		if s.available_gems.black < 4 then None
	    else Some (take_gem 2 Black p s.available_gems)
	| White ->
		if s.available_gems.white < 4 then None
	    else Some (take_gem 2 White p s.available_gems)
	in
	match tup with
	| Some(p, g) ->
					Some {players = List.tl s.players @ [p];
						(** a list of the players playing the game *)
					tier1_deck = s.tier1_deck;
					tier2_deck = s.tier2_deck;
					tier3_deck = s.tier3_deck;
						(** the cards remaining in the decks of cards, seperated by tier *)
					tier1 = s.tier1;
					tier2 = s.tier2;
					tier3 = s.tier3;
						(** the cards available, seperated by tier *)
					nobles = s.nobles;
						(** the nobles currently available *)
					available_gems = g;
						(** the gems currently available for taking *)
					gem_piles = s.gem_piles;
					(** used for checking how many gems a player can/must take if there are
					less than three piles *)
					turns_taken = s.turns_taken + 1;
					(** used for ai behavior *)
					gold = s.gold;
					(** the number of gold coins available **)
				}
	| None -> None

let reserve_card p s c =
	if List.length p.reserved >= 3 then None
	else
	let updated_player = {
			 gems_held = p.gems_held;
			 discounts = p.discounts;
			 points = p.points;
			 bought = p.bought;
			 gold = p.gold + (if s.gold > 0 then 1 else 0);
			 player_type = p.player_type;
			 reserved = c::(p.reserved);
	}
	in
	if exists c s.tier1 then
		   	 let (deck,table) = refresh_cards s.tier1_deck s.tier1 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = table;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }
		 else if exists c s.tier2 then
			 let (deck,table) = refresh_cards s.tier2_deck s.tier2 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = table;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }
		 else
			 let (deck,table) = refresh_cards s.tier3_deck s.tier3 c in
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = table;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }

let reserve_top p s tier =
	if List.length p.reserved >= 3 then None
	else
	let tier_deck =
	match tier with
	| 1 -> s.tier1_deck
	| 2 -> s.tier2_deck
	| _ -> s.tier3_deck
	in
	let updated_player = {
			 gems_held = p.gems_held;
			 discounts = p.discounts;
			 points = p.points;
			 bought = p.bought;
			 gold = p.gold + (if s.gold > 0 then 1 else 0);
			 player_type = p.player_type;
			 reserved = (List.hd tier_deck)::(p.reserved);
	}
	in
	if tier = 1 then
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = (List.tl tier_deck);
				 tier2_deck = s.tier2_deck;
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }
		 else if tier = 2 then
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = (List.tl tier_deck);
				 tier3_deck = s.tier3_deck;
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }
		 else
			 let new_p_list = List.tl s.players @ [updated_player] in
			 Some {
				 players = new_p_list;
					 (** a list of the players playing the game *)
				 tier1_deck = s.tier1_deck;
				 tier2_deck = s.tier2_deck;
				 tier3_deck = (List.tl tier_deck);
					 (** the cards remaining in the decks of cards, seperated by tier *)
				 tier1 = s.tier1;
				 tier2 = s.tier2;
				 tier3 = s.tier3;
					 (** the cards available, seperated by tier *)
				 nobles = s.nobles;
					 (** the nobles currently available *)
				 available_gems = s.available_gems;
					 (** the gems currently available for taking *)
				 gem_piles = s.gem_piles;
				 (** used for checking how many gems a player can/must take if there are
				 less than three piles *)
				 turns_taken = s.turns_taken + 1;
				 (** used for ai behavior *)
				 gold = if s.gold > 0 then (s.gold - 1) else 0;
				 (** the number of gold coins available **)
			 }

let check_nobles p s nob =
	let rec noble_check player st nobles =
	match nobles with
	| [] -> []
	| h::t -> let disc = player.discounts in
			  if disc.red >= h.red
			  && disc.blue >= h.blue
			  && disc.black >= h.black
			  && disc.green >= h.green
			  && disc.white >= h.white
			  then h::(noble_check player st t)
			  else (noble_check player st t)
	in
	match noble_check p s nob with
	| [] -> None
	| l -> Some l

let rec end_game p s turns =
	failwith "Unimplemented"

(************************************)
(* Unfinished play - need to have better logic for when None is returned
 * player should be able to do another move *)
let rec play s m =
	match m with
	| Three (x, y, z) ->
	   take_three_gems (List.hd s.players) s x y z
	| Two (x) ->
	   let ret_val = take_two_gems (List.hd s.players) s x in
		 if ret_val = None then
		 (s, "Illegal move. There are not 4 of this color gem in the pile.")
		 else ((get ret_val), "")
	| Buy(x) ->
	   let ret_val = buy_card (List.hd s.players) s x in
		 if ret_val = None then
		 (s, "Illegal move. You do not have enough gems to buy this card.")
		 else ((get ret_val), "")
	| Reserve(x) ->
	   let ret_val = reserve_card (List.hd s.players) s x in
	   	 if ret_val = None then
		 (s, "Illegal move. You cannot have more than 3 cards reserved at a time.")
		 else ((get ret_val), "")
	| Top(x) ->
	   let ret_val = reserve_top (List.hd s.players) s x in
	   	 if ret_val = None then
		 (s, "Illegal move. You cannot have more than 3 cards reserved at a time.")
		 else ((get ret_val), "")
 	| Pass -> let st = {players = List.tl s.players @ [List.hd s.players];
						(** a list of the players playing the game *)
					tier1_deck = s.tier1_deck;
					tier2_deck = s.tier2_deck;
					tier3_deck = s.tier3_deck;
						(** the cards remaining in the decks of cards, seperated by tier *)
					tier1 = s.tier1;
					tier2 = s.tier2;
					tier3 = s.tier3;
						(** the cards available, seperated by tier *)
					nobles = s.nobles;
						(** the nobles currently available *)
					available_gems = s.available_gems;
						(** the gems currently available for taking *)
					gem_piles = s.gem_piles;
					(** used for checking how many gems a player can/must take if there are
					less than three piles *)
					turns_taken = s.turns_taken + 1;
					(** used for ai behavior *)
					gold = s.gold;
					(** the number of gold coins available **)
				}
			in (st, "")
