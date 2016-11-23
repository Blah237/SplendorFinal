open Card
open Ai

let make_card color points gem_cost = 
	{color = color;
	points = points;
	gem_cost = gem_cost}

let make_tier_1 () = 
	let one = make_card White 0
	{red = 0;
	blue = 3;
	black = 0;
	green = 0;
	white = 0} in
	let two = make_card White 0 
	{red = 2;
	blue = 0;
	black = 1;
	green = 0;
	white = 0} in
	let three = make_card White 0 
	{red = 1;
	blue = 1;
	black = 1;
	green = 1;
	white = 0} in
	let four = make_card White 0 
	{red = 0;
	blue = 2;
	black = 2;
	green = 0;
	white = 0} in
	let five = make_card White 1 
	{red = 0;
	blue = 0;
	black = 0;
	green = 4;
	white = 0} in
	let six = make_card White 0 
	{red = 1;
	blue = 1;
	black = 1;
	green = 2;
	white = 0} in
	let seven = make_card White 0 
	{red = 0;
	blue = 2;
	black = 1;
	green = 2;
	white = 0} in
	let eight = make_card White 0 
	{red = 0;
	blue = 1;
	black = 1;
	green = 0;
	white = 3} in
	let nine = make_card Blue 0 
	{red = 0;
	blue = 0;
	black = 2;
	green = 0;
	white = 1} in
	let ten = make_card Blue 0 
	{red = 0;
	blue = 0;
	black = 3;
	green = 0;
	white = 0} in
	let eleven = make_card Blue 0 
	{red = 1;
	blue = 0;
	black = 1;
	green = 1;
	white = 1} in
	let twelve = make_card Blue 0 
	{red = 0;
	blue = 0;
	black = 2;
	green = 2;
	white = 0} in
	let thirteen = make_card Blue 0 
	{red = 4;
	blue = 0;
	black = 0;
	green = 0;
	white = 0} in


let init_state starting_player num_human num_ai = 
	failwith "Unimplemented"

let buy_card p c = 
	failwith "Unimplemented"

let take_three_gems p g1 g2 g3 = 
	failwith "Unimplemented"

let take_two_gems p gem = 
	failwith "Unimplemented"

let reserve_card p c = 
	failwith "Unimplemented"

let reserve_top p tier = 
	failwith "Unimplemented"

let rec check_nobles p nobles = 
	failwith "Unimplemented"

let rec end_game p turns = 
	failwith "Unimplemented"

(************************************)

let rec play s m = 
	failwith "Unimplemented"