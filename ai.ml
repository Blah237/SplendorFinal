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
	let white_count_2 = gather_colors_white s.tier1 in
	let white_count_3 = gather_colors_white s.tier1 in
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


let get_ordered_color_list_from_tier color tier = 
	let colorlist = get_color_from_tier color [] tier in
	let costlist = get_costs colorlist [] in
	let orderedcostlist = List.sort compare costlist in
	let correct_orderedcostlist = List.rev orderedcostlist in
	order_list colorlist correct_orderedcostlist []


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
	let red_cost = c.gem_cost.red - ai.gems_held.red in
	let blue_cost = c.gem_cost.blue - ai.gems_held.blue in
	let green_cost = c.gem_cost.green - ai.gems_held.green in
	let black_cost = c.gem_cost.black - ai.gems_held.white in
	let white_cost = c.gem_cost.white - ai.gems_held.white in
	{red = red_cost;
	blue = blue_cost;
	black = black_cost;
	green = green_cost;
	white = white_cost}


let determine_gem_goal ai c = 
	failwith "Unimplemented"


let determine_move move s = 
	failwith "Unimplemented"