type color = 
| Red
| Blue
| Black
| Green
| White

type card = {
	color : color;
	points : int;
	red_cost : int;
	blue_cost : int;
	black_cost : int;
	green_cost : int;
	white_cost : int;
}