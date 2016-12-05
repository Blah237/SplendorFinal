(** CITATIONS:

The website below was relied on heavily to write the sublist function:
*http://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml*)

open Card

(********* GENERAL USE FUNCTIONS BELOW *********)

let rec sum_list thelist acc =
	match thelist with
	| [] -> acc
	| hd::tl -> let new_acc = acc + hd in
				sum_list tl new_acc

(** I wrote this function after looking at the following page on Stack Overflow:
* http://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml*)
let rec sublist start theend thelist =
  match thelist with
  | [] -> failwith "range too large"
  | hd::tl ->
     let thetail = if theend = 0 then [] else
     	sublist (start-1) (theend-1) tl in
     if start > 0 then thetail else hd::thetail

(******* GENERAL USE AI-SPECIFIC FUNCTIONS BELOW *******)

(** make a list from a record of gems *)
let make_list_of_record thegems =
	let thered = thegems.red in
	let theblue = thegems.blue in
	let theblack = thegems.black in
	let thegreen = thegems.green in
	let thewhite = thegems.white in
	[thered;theblue;theblack;thegreen;thewhite]

(** determine the remaining cost of a card given the ai's current
 * discounts and gems *)
let cost_remaining ai c : gems =
	let red_cost = c.gem_cost.red - ai.gems_held.red - ai.discounts.red in
	let blue_cost = c.gem_cost.blue - ai.gems_held.blue  - ai.discounts.blue in
	let green_cost = c.gem_cost.green - ai.gems_held.green
	- ai.discounts.green in
	let black_cost = c.gem_cost.black - ai.gems_held.black
	- ai.discounts.black in
	let white_cost = c.gem_cost.white - ai.gems_held.white  -
    ai.discounts.white in
	{red = red_cost;
	blue = blue_cost;
	black = black_cost;
	green = green_cost;
	white = white_cost}

(** totals the cost of a single card*)
let total_cost c =
	c.gem_cost.red +
	c.gem_cost.blue +
	c.gem_cost.green +
	c.gem_cost.black +
	c.gem_cost.white

(** totals the number of gems in a single record of gems*)
let total_cost_gems thegems =
	thegems.red +
	thegems.blue +
	thegems.green +
	thegems.black +
	thegems.white

(** determines whether a card can be bought if the ai could take an infinite
* number of turns, bar reserving *)
let is_feasible s ai c =
	let remainingcost = cost_remaining ai c in
	let possible_cost =
	{red = remainingcost.red - s.available_gems.red;
	 blue = remainingcost.blue - s.available_gems.blue;
	 black = remainingcost.black - s.available_gems.black;
	 green = remainingcost.green - s.available_gems.green;
	 white = remainingcost.white - s.available_gems.white} in
	let totalgemcost = total_cost_gems possible_cost in
	if totalgemcost <= 0 then true else false

(** removes all non-feasible cards from a list of cards *)
let rec remove_not_feasible s clist ai acc =
	match clist with
	| [] -> acc
	| hd::tl -> if is_feasible s ai hd then
				let new_acc = acc @ [hd] in
				remove_not_feasible s tl ai new_acc
				else remove_not_feasible s tl ai acc

(********* DETERMINE DOMINANT COLOR FUNCTIONS BELOW ***********)

let rec gather_colors_red deck=
	match deck with
	| [] -> 0
	| hd::tl -> hd.gem_cost.red + gather_colors_red tl

let rec gather_colors_blue deck=
	match deck with
	| [] -> 0
	| hd::tl -> hd.gem_cost.blue + gather_colors_blue tl

let rec gather_colors_black deck=
	match deck with
	| [] -> 0
	| hd::tl -> hd.gem_cost.black + gather_colors_black tl

let rec gather_colors_green deck=
	match deck with
	| [] -> 0
	| hd::tl -> hd.gem_cost.green + gather_colors_green tl

let rec gather_colors_white deck=
	match deck with
	| [] -> 0
	| hd::tl -> hd.gem_cost.white + gather_colors_white tl

(** determine the dominant colors on the board*)
let determine_domininant_color s =
	let red_count_1 = gather_colors_red s.tier1 in
	let red_count_2 = gather_colors_red s.tier2 in
	let red_count_3 = gather_colors_red s.tier3 in
	let blue_count_1 = gather_colors_blue s.tier1 in
	let blue_count_2 = gather_colors_blue s.tier2 in
	let blue_count_3 = gather_colors_blue s.tier3 in
	let green_count_1 = gather_colors_green s.tier1 in
	let green_count_2 = gather_colors_green s.tier2 in
	let green_count_3 = gather_colors_green s.tier3 in
	let black_count_1 = gather_colors_black s.tier1 in
	let black_count_2 = gather_colors_black s.tier2 in
	let black_count_3 = gather_colors_black s.tier3 in
	let white_count_1 = gather_colors_white s.tier1 in
	let white_count_2 = gather_colors_white s.tier2 in
	let white_count_3 = gather_colors_white s.tier3 in
	let total_red = red_count_1 + red_count_2 + red_count_3 in
	let total_blue = blue_count_1 + blue_count_2 + blue_count_3 in
	let total_green = green_count_1 + green_count_2 + green_count_3 in
	let total_black = black_count_1 + black_count_2 + black_count_3 in
	let total_white = white_count_1 + white_count_2 + white_count_3 in
	let thelist = [total_red; total_green; total_blue; total_black;
					 total_white] in
	let sorted = List.sort compare thelist in
	let correct = List.rev sorted in
	let redbool = true in
	let bluebool = true in
	let greenbool = true in
	let blackbool = true in
	let first = (if List.hd correct = total_red  && redbool then Red else if
				List.hd correct = total_blue && bluebool then Blue else if
				List.hd correct = total_green && greenbool then Green else if
				List.hd correct = total_black && blackbool then Black else
				White) in
	let redbool = (match first with Red -> false | _ -> true) in
	let bluebool = (match first with Blue -> false | _ -> true) in
	let greenbool = (match first with Green -> false | _ -> true) in
	let blackbool = (match first with Black -> false | _ -> true) in
	let second = (if List.nth correct 1 = total_red && redbool then Red else if
				List.nth correct 1 = total_blue && bluebool then Blue else if
				List.nth correct 1 = total_green && greenbool then Green else if
				List.nth correct 1 = total_black && blackbool then Black else
				White) in
	let redbool = (match second with Red -> false | _ -> true) in
	let bluebool = (match second with Blue -> false | _ -> true) in
	let greenbool = (match second with Green -> false | _ -> true) in
	let blackbool = (match second with Black -> false | _ -> true) in
	let third = (if List.nth correct 2 = total_red && redbool then Red else if
				List.nth correct 2 = total_blue && bluebool then Blue else if
				List.nth correct 2 = total_green && greenbool then Green else if
				List.nth correct 2 = total_black && blackbool then Black else
				White) in
	let redbool = (match third with Red -> false | _ -> true) in
	let bluebool = (match third with Blue -> false | _ -> true) in
	let greenbool = (match third with Green -> false | _ -> true) in
	let blackbool = (match third with Black -> false | _ -> true) in
	let fourth = (if List.nth correct 3 = total_red && redbool then Red else if
				List.nth correct 3 = total_blue && bluebool then Blue else if
				List.nth correct 3 = total_green && greenbool then Green else if
				List.nth correct 3 = total_black && blackbool then Black else
				White) in
	let redbool = (match fourth with Red -> false | _ -> true) in
	let bluebool = (match fourth with Blue -> false | _ -> true) in
	let greenbool = (match fourth with Green -> false | _ -> true) in
	let blackbool = (match fourth with Black -> false | _ -> true) in
	let fifth = (if List.nth correct 4 = total_red && redbool then Red else if
				List.nth correct 4 = total_blue && bluebool then Blue else if
				List.nth correct 4 = total_green && greenbool then Green else if
				List.nth correct 4 = total_black && blackbool then Black else
				White) in
	[first;second;third;fourth;fifth]

(****** DETERMINE GOAL HELPER FUNCTIONS ******)

let rec get_color_from_tier color acc tier =
	match tier with
	| [] -> acc
	| hd::tl -> if hd.color = color then
				let new_acc = hd::acc in
				get_color_from_tier color new_acc tl
				else get_color_from_tier color acc tl

let rec get_costs coloredlist ai acc =
	match coloredlist with
	| [] -> acc
	| hd::tl -> let thecostgems = cost_remaining ai hd in
				let cost = total_cost_gems thecostgems in
				let new_acc = cost::acc in
				get_costs tl ai new_acc

(** orders a list of cards according to ordered costs (costs
* should be ordered from greatest to least*)
let rec order_list coloredlist orderedcostlist ai acc =
	match coloredlist with
	| [] -> acc
	| hd::tl ->
	let remainingcost = cost_remaining ai hd in
	let thecost = total_cost_gems remainingcost in
	if List.hd orderedcostlist = thecost then
			  let new_acc = hd::acc in
			  let new_orderedcostlist = List.tl orderedcostlist in
			  order_list tl new_orderedcostlist ai new_acc
			  else
			  	let new_coloredlist = tl @ [hd] in
			  	order_list new_coloredlist orderedcostlist ai acc

(********* DETERMINE EARLY GOAL FUNCTIONS ***********)

(** get the goals of a specific color from a list of cards*)
let get_ordered_color_list_from_tier color tier ai =
	let colorlist = get_color_from_tier color [] tier in
	let costlist = get_costs colorlist ai [] in
	let orderedcostlist = List.sort compare costlist in
	let correct_orderedcostlist = List.rev orderedcostlist in
	order_list colorlist correct_orderedcostlist ai []

(** determine the list of goals for the early game, dependent on dominant colors*)
let rec determine_early_goal s ai colorlist acc =
	match colorlist with
	| [] -> acc
	| hd::tl ->
		(match hd with
		| Red -> let thelist = get_ordered_color_list_from_tier Red s.tier1 ai in
				 let new_acc = acc @ thelist in
				 determine_early_goal s ai tl new_acc
		| Blue -> let thelist = get_ordered_color_list_from_tier Blue s.tier1 ai in
				  let new_acc = acc @ thelist in
				  determine_early_goal s ai tl new_acc
		| Green -> let thelist = get_ordered_color_list_from_tier Green s.tier1 ai in
				   let new_acc = acc @ thelist in
				   determine_early_goal s ai tl new_acc
		| Black -> let thelist = get_ordered_color_list_from_tier Black s.tier1 ai in
				   let new_acc = acc @ thelist in
				   determine_early_goal s ai tl new_acc
		| White -> let thelist = get_ordered_color_list_from_tier White s.tier1 ai in
				   let new_acc = acc @ thelist in
				   determine_early_goal s ai tl new_acc)


(*** DETERMINE LATE GOAL FUNCTIONS ***)

(** get cards of a certain point value from the board*)
let rec get_number (points : int) (board : card list) (acc : card list)
: card list =
	match board with
	| [] -> acc
	| hd::tl -> if hd.points = points then
				let new_acc = hd::acc in
				get_number points tl new_acc
				else get_number points tl acc

(** determines the list of goals (of cards) for the late game*)
let determine_late_goal s ai =
	let board = s.tier1 @ s.tier2 @ s.tier3 in
	let number_5 = get_number 5 board [] in
	let number_4 = get_number 4 board [] in
	let number_3 = get_number 3 board [] in
	let number_2 = get_number 2 board [] in
	let number_1 = get_number 1 board [] in
	let costlist_5 = get_costs number_5 ai [] in
	let costlist_4 = get_costs number_4 ai [] in
	let costlist_3 = get_costs number_3 ai [] in
	let costlist_2 = get_costs number_2 ai [] in
	let costlist_1 = get_costs number_1 ai [] in
	let orderedcostlist_5 = List.sort compare costlist_5 in
	let orderedcostlist_4 = List.sort compare costlist_4 in
	let orderedcostlist_3 = List.sort compare costlist_3 in
	let orderedcostlist_2 = List.sort compare costlist_2 in
	let orderedcostlist_1 = List.sort compare costlist_1 in
	let correct5 = List.rev orderedcostlist_5 in
	let correct4 = List.rev orderedcostlist_4 in
	let correct3 = List.rev orderedcostlist_3 in
	let correct2 = List.rev orderedcostlist_2 in
	let correct1 = List.rev orderedcostlist_1 in
	let goal5 = order_list number_5 correct5 ai [] in
	let goal4 = order_list number_4 correct4 ai [] in
	let goal3 = order_list number_3 correct3 ai [] in
	let goal2 = order_list number_2 correct2 ai [] in
	let goal1 = order_list number_1 correct1 ai [] in
	goal5 @ goal4 @ goal3 @ goal2 @ goal1


(***** DETERMINE GEM GOAL FUNCTIONS ******)

let rec remove_zero_costs costlist acc =
	match costlist with
	| [] -> acc
	| (a,b)::tl -> if b <= 0 then remove_zero_costs tl acc else
				let new_acc = (a,b)::acc in
				remove_zero_costs tl new_acc

let make_paired_list costrecord =
	let thered = (Red, costrecord.red) in
	let theblue = (Blue, costrecord.blue) in
	let theblack = (Black, costrecord.black) in
	let thegreen = (Green, costrecord.green) in
	let thewhite = (White, costrecord.white) in
	[thered;theblue;theblack;thegreen;thewhite]

let rec make_cost_list costpairs acc =
	match costpairs with
	| [] -> acc
	| (a,b)::tl -> let new_acc = b::acc in
				   make_cost_list tl new_acc

let rec order_positive_costs orderedcostlist costpairlist acc =
	match orderedcostlist with
	| [] -> acc
	| hd::tl -> match costpairlist with
				| [] -> acc
				| (a,b)::tl2 -> if hd = b then
					let new_acc = a::acc in
					order_positive_costs tl tl2 new_acc
				else
					let new_coloredlist = tl2 @ [(a,b)] in
					order_positive_costs orderedcostlist new_coloredlist acc


(** determines the gems an ai should take based on card goals*)
let rec determine_gem_goal ai clist acc =
	match clist with
	| [] -> acc
	| hd::tl ->
		let remainingcost = cost_remaining ai hd in
		let pairedlist = make_paired_list remainingcost in
		let zero_removed = remove_zero_costs pairedlist [] in
		let costlist = make_cost_list zero_removed [] in
		let pre_ordered_cost_list = List.sort compare costlist in
		let orderedcostlist = List.rev pre_ordered_cost_list in
		let thelist = order_positive_costs orderedcostlist zero_removed [] in
		let new_acc = acc @ thelist in
		determine_gem_goal ai tl new_acc

(**** PURCHASE CARDS HELPER FUNCTIONS ****)

(** returns [true] if a card is purchasable with the ai's current gems, and
* [false] if not *)
let is_purchasable ai c =
	let theremainingcost = cost_remaining ai c in
	if theremainingcost.red <= 0 
	&& theremainingcost.blue <= 0
	&& theremainingcost.green <= 0
	&& theremainingcost.black <= 0
	&& theremainingcost.white <= 0
	then true else false

	(*let pairs = make_paired_list remainingcost in
	let no_zeroes = remove_zero_costs pairs [] in
	if List.length no_zeroes = 0 then true else false*)

(** returns [Some Buy card] where card is the first card in the list of goals
 * that the ai can purcahse, and [None] if no card in the goals list is
 * purchasable *)
let rec try_purchase_all ai orderedgoals =
	match orderedgoals with
	| [] -> None
	| hd::tl -> if is_purchasable ai hd then
				let the_move = Buy hd in Some the_move
				else try_purchase_all ai tl

(****** GEM MOVE FUNCTIONS ******)

(** determines the move the ai should make when there is only one
* gem pile remaining *)
let rec gem_piles_1 thegemspairedlist =
	match thegemspairedlist with
	| [] -> Pass
	| (a,b)::tl -> if b <> 0 then Three (a,None,None) else gem_piles_1 tl

(** determines the move the ai should make when there are only two
* gem piles remaining *)
let rec gem_piles_2 thegemspairedlist acc =
	if List.length acc = 2 then
	let thefirst = List.nth acc 0 in
	let thesecond = List.nth acc 1 in
	Three (thefirst, Some thesecond, None) else
	match thegemspairedlist with
	| [] -> Pass
	| (a,b)::tl -> if b = 0 then gem_piles_2 tl acc else
				   let new_acc = a::acc in
				   gem_piles_2 tl new_acc

(** determines the move the ai should make when there are only three
* gem piles remaining *)
let rec gem_piles_3 thegemspairedlist acc =
	if List.length acc = 3 then
	let thefirst = List.nth acc 0 in
	let thesecond = List.nth acc 1 in
	let thethird = List.nth acc 2 in
	Three (thefirst, Some thesecond, Some thethird) else
	match thegemspairedlist with
	| [] -> Pass
	| (a,b)::tl -> if b = 0 then gem_piles_3 tl acc else
				   let new_acc = a::acc in
				   gem_piles_3 tl new_acc

(** determines ai behavior for a non-edge case "take three gems" move, where
* it does not have 8 or more gems and there at least 4 gem piles remaining *)
let rec normal_gem_move available gemgoals acc 
redbool bluebool greenbool whitebool blackbool =
	if List.length acc = 3 then
	let thefirst = List.nth acc 0 in
	let thesecond = List.nth acc 1 in
	let thethird = List.nth acc 2 in
	Three (thefirst, Some thesecond, Some thethird)
	else
	(match gemgoals with
	| [] ->
		if List.length acc = 2 then
			let thefirst = List.nth acc 0 in
			let thesecond = List.nth acc 1 in
			Three (thefirst, Some thesecond, None)
		else if List.length acc = 1 then
			let thefirst = List.nth acc 0 in
			Three (thefirst, None, None)
		else Pass
	| hd::tl ->
	(match hd with
	| Red -> if available.red > 0 && redbool then let new_acc = hd::acc in
								  let new_available =
								  {red = available.red - 1;
								  blue = available.blue;
								  black = available.black;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc 
								  false bluebool greenbool whitebool blackbool
			 else normal_gem_move available tl acc redbool bluebool greenbool whitebool blackbool
	| Blue -> if available.blue > 0 && bluebool then let new_acc = hd::acc in
								  let new_available =
								  {red = available.red;
								  blue = available.blue - 1;
								  black = available.black;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
								  redbool false greenbool whitebool blackbool
			  else normal_gem_move available tl acc redbool bluebool greenbool whitebool blackbool
	| Black -> if available.black > 0 && blackbool then let new_acc = hd::acc in
								  let new_available =
								  {red = available.red;
								  blue = available.blue;
								  black = available.black - 1;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
								  redbool bluebool greenbool whitebool false
			   else normal_gem_move available tl acc redbool bluebool greenbool whitebool blackbool
	| Green -> if available.green > 0 && greenbool then let new_acc = hd::acc in
								  let new_available =
								  {red = available.red;
								  blue = available.blue;
								  black = available.black;
								  green = available.green - 1;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
								  redbool bluebool false whitebool blackbool
			   else normal_gem_move available tl acc redbool bluebool greenbool whitebool blackbool
	| White -> if available.white > 0 && whitebool then let new_acc = hd::acc in
								  let new_available =
								  {red = available.red;
								  blue = available.blue;
								  black = available.black;
								  green = available.green;
								  white = available.white - 1} in
								  normal_gem_move new_available tl new_acc
								  redbool bluebool greenbool false blackbool
               else normal_gem_move available tl acc redbool bluebool greenbool whitebool blackbool))


(**** DETERMINE MOVE FUNCTIONS *****)

(** determines the move an ai should make in the early game, using dominant
* colors and only looking at tier 1*)
let determine_early_move s ai =
	let dominants = determine_domininant_color s in
	let pre_early_goal = determine_early_goal s ai dominants [] in
	let early_goal = remove_not_feasible s pre_early_goal ai [] in
	let primary_goal = List.hd early_goal in
	let basic_list_of_gems = make_list_of_record ai.gems_held in
	let total_ai_gems = sum_list basic_list_of_gems 0 in
	let paired_list = make_paired_list s.available_gems in
	if is_purchasable ai primary_goal then Buy primary_goal else
	if s.gem_piles <= 3 then
	let zero_gem_move = try_purchase_all ai early_goal in
	match zero_gem_move with
	| Some x -> x
	| None ->
	if s.gem_piles = 0 then Pass else
	if s.gem_piles = 1 then gem_piles_1 paired_list else
	if s.gem_piles = 2 then gem_piles_2 paired_list [] else
	gem_piles_3 paired_list [] else
    if total_ai_gems > 7 then let zero_gem_move =
    							try_purchase_all ai early_goal in
    match zero_gem_move with
    | Some x -> x
    | None -> let thegemgoals = determine_gem_goal ai early_goal [] in
    		  normal_gem_move s.available_gems thegemgoals [] true true true true true
    else
    	let thegemgoals = determine_gem_goal ai early_goal [] in
    		  normal_gem_move s.available_gems thegemgoals [] true true true true true

(** Determines the move an ai should make in the late game. The ai will
* seek to the highest point value card that is feasible based on the
* current board state. *)
let determine_late_move s ai =
	let pre_late_goal = determine_late_goal s ai in
	let late_goal = remove_not_feasible s pre_late_goal ai [] in
	let primary_goal = List.hd late_goal in
	let basic_list_of_gems = make_list_of_record ai.gems_held in
	let total_ai_gems = sum_list basic_list_of_gems 0 in
	let paired_list = make_paired_list s.available_gems in
	if is_purchasable ai primary_goal then Buy primary_goal else
	if s.gem_piles <= 3 then
	let zero_gem_move = try_purchase_all ai late_goal in
	match zero_gem_move with
	| Some x -> x
	| None ->
	if s.gem_piles = 0 then Pass else
	if s.gem_piles = 1 then gem_piles_1 paired_list else
	if s.gem_piles = 2 then gem_piles_2 paired_list [] else
	gem_piles_3 paired_list [] else
    if total_ai_gems > 7 then let zero_gem_move =
    							try_purchase_all ai late_goal in
    match zero_gem_move with
    | Some x -> x
    | None -> let thegemgoals = determine_gem_goal ai late_goal [] in
    		  normal_gem_move s.available_gems thegemgoals [] true true true true true
    else
    	let thegemgoals = determine_gem_goal ai late_goal [] in
    		  normal_gem_move s.available_gems thegemgoals [] true true true true true


(***************************** DETERMINE MOVE *******************************)
let determine_move s =
	let num_players = List.length s.players in
	let current_player = List.hd s.players in
	if num_players = 4 then
		if s.turns_taken <= 40 then determine_early_move s current_player
		else determine_late_move s current_player
	else if num_players = 3 then
		if s.turns_taken <= 30 then determine_early_move s current_player
		else determine_late_move s current_player
	else
		if s.turns_taken <= 20 then determine_early_move s current_player
		else determine_late_move s current_player
(****************************************************************************)


(**** DISCARD GEMS FUNCTIONS ****)
let take_three_gems_helper_helper ai three =
	match ai.player_type with
	| Human -> failwith "wrong player type"
	| Ai thelist -> match three with
					| Three (g1, Some g2, Some g3) -> thelist @ [g3;g2;g1]
					| Three (g1, Some g2, None) -> thelist @ [g2;g1]
					| Three (g1, None, None) -> thelist @ [g1]
					| _ -> failwith "Wrong move"

let take_three_gems_helper ai three =
	let new_list = take_three_gems_helper_helper ai three in
	let corrected = if List.length new_list >= 50 then
	sublist 0 46 new_list else new_list in
	{name = ai.name;
	gems_held = ai.gems_held;
	discounts = ai.discounts;
	reserved = ai.reserved;
	bought = ai.bought;
	points = ai.points;
	player_type = Ai corrected;
	gold = ai.gold}

let determine_discard s =
	let current_player = List.hd s.players in
	match current_player.player_type with
	| Human -> failwith "Wrong player type for call"
	| Ai thelist -> let thesize = List.length thelist in
					let correctedsize = thesize - 1 in
					List.nth thelist correctedsize
