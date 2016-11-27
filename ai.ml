open Card

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

(** Determine the dominant colors on the board*)
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
	let first = (if List.hd correct = total_red then Red else if
				List.hd correct = total_blue then Blue else if
				List.hd correct = total_green then Green else if
				List.hd correct = total_black then Black else
				White) in
	let second = (if List.nth correct 1 = total_red then Red else if
				List.nth correct 1 = total_blue then Blue else if
				List.nth correct 1 = total_green then Green else if
				List.nth correct 1 = total_black then Black else
				White) in
	let third = (if List.nth correct 2 = total_red then Red else if
				List.nth correct 2 = total_blue then Blue else if
				List.nth correct 2 = total_green then Green else if
				List.nth correct 2 = total_black then Black else
				White) in
	let fourth = (if List.nth correct 3 = total_red then Red else if
				List.nth correct 3 = total_blue then Blue else if
				List.nth correct 3 = total_green then Green else if
				List.nth correct 3 = total_black then Black else
				White) in
	let fifth = (if List.nth correct 4 = total_red then Red else if
				List.nth correct 4 = total_blue then Blue else if
				List.nth correct 4 = total_green then Green else if
				List.nth correct 4 = total_black then Black else
				White) in
	[first;second;third;fourth;fifth]

let total_cost c = 
	c.gem_cost.red +
	c.gem_cost.blue +
	c.gem_cost.green +
	c.gem_cost.black +
	c.gem_cost.white

let rec get_color_from_tier color acc tier = 
	match tier with
	| [] -> acc
	| hd::tl -> if hd.color = color then
				let new_acc = hd::acc in
				get_color_from_tier color new_acc tl
				else get_color_from_tier color acc tl

let rec get_costs coloredlist acc =
	match coloredlist with
	| [] -> acc
	| hd::tl -> let cost = total_cost hd in
				let new_acc = cost::acc in
				get_costs tl new_acc

let rec order_list coloredlist orderedcostlist acc =
	match coloredlist with
	| [] -> acc
	| hd::tl -> if List.hd orderedcostlist = total_cost hd then
			  let new_acc = hd::acc in
			  let new_orderedcostlist = List.tl orderedcostlist in
			  order_list tl new_orderedcostlist new_acc
			  else 
			  	let new_coloredlist = tl @ [hd] in
			  	order_list new_coloredlist orderedcostlist acc

(** get the goals of a specific color from a list of cards*)
let get_ordered_color_list_from_tier color tier = 
	let colorlist = get_color_from_tier color [] tier in
	let costlist = get_costs colorlist [] in
	let orderedcostlist = List.sort compare costlist in
	let correct_orderedcostlist = List.rev orderedcostlist in
	order_list colorlist correct_orderedcostlist []

(** determine the list of goals for the early game, dependent on dominant colors*)
let rec determine_early_goal s colorlist acc =
	match colorlist with
	| [] -> acc
	| hd::tl -> 
		(match hd with
		| Red -> let thelist = get_ordered_color_list_from_tier Red s.tier1 in
				 let new_acc = acc @ thelist in
				 determine_early_goal s tl new_acc
		| Blue -> let thelist = get_ordered_color_list_from_tier Blue s.tier1 in
				  let new_acc = acc @ thelist in
				  determine_early_goal s tl new_acc
		| Green -> let thelist = get_ordered_color_list_from_tier Green s.tier1 in
				   let new_acc = acc @ thelist in
				   determine_early_goal s tl new_acc
		| Black -> let thelist = get_ordered_color_list_from_tier Black s.tier1 in
				   let new_acc = acc @ thelist in
				   determine_early_goal s tl new_acc
		| White -> let thelist = get_ordered_color_list_from_tier White s.tier1 in
				   let new_acc = acc @ thelist in
				   determine_early_goal s tl new_acc)


let cost_remaining ai c = 
	let red_cost = c.gem_cost.red - ai.gems_held.red - ai.discounts.red in
	let blue_cost = c.gem_cost.blue - ai.gems_held.blue  - ai.discounts.blue in
	let green_cost = c.gem_cost.green - ai.gems_held.green 
	- ai.discounts.green in
	let black_cost = c.gem_cost.black - ai.gems_held.white 
	- ai.discounts.white in
	let white_cost = c.gem_cost.white - ai.gems_held.white  -
    ai.discounts.black in
	{red = red_cost;
	blue = blue_cost;
	black = black_cost;
	green = green_cost;
	white = white_cost}

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
		let orderedcostlist = List.sort compare costlist in
		let thelist = order_positive_costs orderedcostlist zero_removed [] in
		let new_acc = acc @ thelist in
		determine_gem_goal ai tl new_acc

let is_purchasable ai c = 
	let remainingcost = cost_remaining ai c in
	let pairs = make_paired_list remainingcost in
	let no_zeroes = remove_zero_costs pairs [] in
	if List.length no_zeroes = 0 then true else false

let rec try_purchase_all ai orderedgoals = 
	match orderedgoals with
	| [] -> None
	| hd::tl -> if is_purchasable ai hd then 
				let the_move = Buy hd in Some the_move
				else try_purchase_all ai tl

let make_list_of_record thegems = 
	let thered = thegems.red in
	let theblue = thegems.blue in
	let theblack = thegems.black in
	let thegreen = thegems.green in
	let thewhite = thegems.white in
	[thered;theblue;theblack;thegreen;thewhite]

let rec sum_list thelist acc =
	match thelist with
	| [] -> acc 
	| hd::tl -> let new_acc = acc + hd in
				sum_list tl new_acc

let rec gem_piles_1 thegemspairedlist = 
	match thegemspairedlist with
	| [] -> Pass
	| (a,b)::tl -> if b <> 0 then Three (a,None,None) else gem_piles_1 tl

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


let rec normal_gem_move available gemgoals acc =
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
	| Red -> if available.red > 0 then let new_acc = hd::acc in
								  let new_available = 
								  {red = available.red - 1;
								  blue = available.blue;
								  black = available.black;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
			 else normal_gem_move available tl acc
	| Blue -> if available.blue > 0 then let new_acc = hd::acc in
								  let new_available = 
								  {red = available.red;
								  blue = available.blue - 1;
								  black = available.black;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
			  else normal_gem_move available tl acc
	| Black -> if available.black > 0 then let new_acc = hd::acc in
								  let new_available = 
								  {red = available.red;
								  blue = available.blue;
								  black = available.black - 1;
								  green = available.green;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
			   else normal_gem_move available tl acc
	| Green -> if available.green > 0 then let new_acc = hd::acc in
								  let new_available = 
								  {red = available.red;
								  blue = available.blue;
								  black = available.black;
								  green = available.green - 1;
								  white = available.white} in
								  normal_gem_move new_available tl new_acc
			   else normal_gem_move available tl acc
	| White -> if available.white > 0 then let new_acc = hd::acc in
								  let new_available = 
								  {red = available.red;
								  blue = available.blue;
								  black = available.black;
								  green = available.green;
								  white = available.white - 1} in
								  normal_gem_move new_available tl new_acc
               else normal_gem_move available tl acc))

(** determines the move an ai should make in the early game, using dominant
* colors and only looking at tier 1*)
let determine_early_move s ai = 
	let dominants = determine_domininant_color s in
	let early_goal = determine_early_goal s dominants [] in
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
    if total_ai_gems > 7 then let zero_gem_move = try_purchase_all ai early_goal in
    match zero_gem_move with
    | Some x -> x
    | None -> let thegemgoals = determine_gem_goal ai early_goal [] in
    		  normal_gem_move s.available_gems thegemgoals []
    else 
    	let thegemgoals = determine_gem_goal ai early_goal [] in
    		  normal_gem_move s.available_gems thegemgoals []

   	

let determine_move s = 
	failwith "Unimplemented"