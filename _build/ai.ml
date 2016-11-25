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


let determine_gem_goal c =
	failwith "Unimplemented"

let determine_goal s colorlist =
	failwith "Unimplemented"

let determine_move move s =
	failwith "Unimplemented"
