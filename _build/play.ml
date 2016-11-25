open Card
open Ai

let make_card color points gem_cost =
	{color = color;
	points = points;
	gem_cost = gem_cost}

let make_tier_1 () =
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

	let make_tier_2 () =
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



 let make_tier_3 () =
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


let make_nobles () =
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


let init_state starting_player num_human num_ai =
	failwith "Unimplemented"

let buy_card p s c =
	failwith "Unimplemented"

let take_three_gems p s g1 g2 g3 =
	failwith "Unimplemented"

let take_two_gems p s gem =
	failwith "Unimplemented"

let reserve_card p s c =
	failwith "Unimplemented"

let reserve_top p s tier =
	failwith "Unimplemented"

let rec check_nobles p s nobles =
	failwith "Unimplemented"

let rec end_game p s turns =
	failwith "Unimplemented"

(************************************)

let rec play s m =
	failwith "Unimplemented"
