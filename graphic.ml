open Card
open Play
open Graphics
open Ai

(* colors *)
let red    = rgb 222 27  27
let blue   = rgb 33  149 211
let green  = rgb 59  181 74
let black  = rgb 43  43  43
let white  = rgb 246 246 246
let gold   = rgb 218 165 32
let grey   = rgb 160 160 160
let silver = rgb 192 192 192
let indigo = rgb 75 0 130

(* Window Size *)
let height = 675
let width = 955

(* Card Dimensions *)
let card_height = 115
let card_width = 80
let noble_height = 80
let noble_width = 80

(* Gem Info *)
let left_buffer = 40 (* Space between gems and left edge *)
let top_buffer = 50  (* Space between gems and top *)
let gem_buffer = 75  (* Space between gems themselves *)
let gem_radius = 30  (* Gem radius *)

(* Tier Info *)
let tier_left = 120       (* Space between cards and left edge *)
let tier_top = top_buffer (* Space between cards and top *)
let card_spacing = 10     (* Space between cards *)

(* Deck Info *)
let deck_height = card_height
let deck_width  = 20
let deck_left = 85

(* Player Info *)
let player_height = 150
let player_width = 240
let player_left = 700
let player_buffer = 15

(* Type for all possible clicks on the board *)
type clickable =
| Gem of int
| Card of card
| Deck1
| Deck2
| Deck3
| Buy
| Reserve
| Cancel

(* Int to color *)
let int_to_color x =
match x with
| 3913034 -> Green
| 16185078 -> White
| 2201043 -> Blue
| 2829099 -> Black
| 14555931 -> Red

(* [create_graph] creates a blank graph with grey background *)
let create_graph height width =
  close_graph ();
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "Splendor";
  set_color grey;
  fill_rect 0 0 width height;
  ()

(* [draw_gems state] draws the all remaining gems in [state] *)
let draw_gems state =
  let left = left_buffer in (* Left padding *)
  let radius = gem_radius in (* Radius of circle *)
  let buffer = gem_buffer in (* Space between circles *)
  let top = height - top_buffer in (* Top padding *)
  (* Draws circle of color [color], text color [text], gem [gem], order [num] *)
  let draw_circle gem color text num =
    if gem = 0 then begin (* Don't draw circle if no gems are left *)
      set_color grey;
      fill_circle left (top - num*buffer) radius;
      () end
    else begin
      set_color color;
      fill_circle left (top - num*buffer) radius;
      moveto (left - 2) (top - num*buffer - 5);
      set_color text;
      draw_string (string_of_int gem);
      () end in
  draw_circle state.available_gems.green green black 0;
  draw_circle state.available_gems.white white black 1;
  draw_circle state.available_gems.blue  blue  white 2;
  draw_circle state.available_gems.black black white 3;
  draw_circle state.available_gems.red   red   black 4;
  draw_circle state.gold                 gold  black 5;
  ()

(* [draw_card] draws [card] at location [x],[y].
   If [noble] then change colors to silver and black *)
let draw_card card x y noble =
  let (color,text) = if noble then silver, black else
  match card.color with
    | Red   -> red, black
    | Blue  -> blue, white
    | Black -> black, white
    | Green -> green, black
    | White -> white, black in
  set_color color;
  fill_rect x y card_width card_height;
  moveto (x + 5) (y + card_height - 15);
  set_color text;
  draw_string ("points: " ^ string_of_int card.points);
  moveto x (y + card_height - 20);
  rlineto card_width 0;
  moveto (x + 5) (y + card_height - 35);
  draw_string ("price: ");
  moveto (x + 5) (y + card_height - 45);
  (* Draw a circle of gems only if price > 0 *)
  let draw_circle gem color text num =
    if gem < 1 then () else
    let radius = 10 in
    let top = y + 65 in
    let left = x + 25 in
    let buffer = 25 in
    let more_top = if num > 2 then 13 else 0 in
    let more_left = if num > 2 then buffer else 0 in
    set_color color;
    fill_circle (left + more_left) (top - (num mod 3)*buffer - more_top) radius;
    set_color black;
    draw_circle (left + more_left) (top - (num mod 3)*buffer - more_top) radius;
    moveto (left + more_left - 2) (top - (num mod 3)*buffer - more_top - 5);
    set_color text;
    draw_string (string_of_int gem);
  () in
  draw_circle card.gem_cost.green green black 0;
  draw_circle card.gem_cost.white white black 1;
  draw_circle card.gem_cost.blue  blue  white 2;
  draw_circle card.gem_cost.black black white 3;
  draw_circle card.gem_cost.red   red   black 4;
  ()

(* [draw_noble] draws one [noble] card at location [x],[y] *)
let draw_noble noble x y =
  (* A noble is a card with silver color and 3 points *)
  let temp_card = {
    color = Red; (* random color, doesn't matter *)
    points = 3;
    gem_cost = noble;
  } in
  draw_card temp_card x y true

(* Calc the # of aplayers gems *)
let calc_players_gems gems =
	let r = gems.red in
	let g = gems.green in
	let w = gems.white in
	let b = gems.blue in
	let bl = gems.black in
	let map = [r;g;w;b;bl] in
	List.fold_left (fun d x ->if x > 0 then d+x else d) 0 map

(* [draw_tier] draws all cards in [lst] horizontally starting at [x], [y] *)
let draw_tier tier x y =
  let rec draw_each lst order =
    match lst with
    | []   -> ()
    | h::t -> draw_card h (x + order*(card_width+card_spacing)) y false;
              draw_each t (order + 1)
  in draw_each tier 0

(* [draw_nobles] draws all nobles in [lst] to the right of the other cards *)
let draw_nobles nobles x y =
let rec draw_each lst order =
    match lst with
    | []     -> ()
    | hd::tl ->
      let w = card_width + card_spacing in
      let h = card_height + card_spacing in
      let x, y =
        if order = 0 then x, (y + 2*h)
        else if order = 1 then x, (y + h)
        else if order = 2 then x, y
        else if order = 3 then (x + w), (y + 2*h - card_height/2)
        else if order = 4 then (x + w), (y + h - card_height/2)
        else failwith "You can't have more than 5 nobles in a game"
      in
      draw_noble hd x y;
      draw_each tl (order + 1)
  in draw_each nobles 0

(* [draw_cards state] draws all open cards *)
let draw_cards state =
  let big_space = card_height + card_spacing in
  let top = height - tier_top in
  let bottom = top - 3*big_space in
  draw_tier state.tier1 tier_left (bottom + 0*big_space);
  draw_tier state.tier2 tier_left (bottom + 1*big_space);
  draw_tier state.tier3 tier_left (bottom + 2*big_space);
  draw_nobles state.nobles (tier_left + 3*big_space) bottom ;
  ()

(* [draw_decks] draws the remaining number of cards per deck *)
let draw_decks state =
  let big_space = deck_height + card_spacing in
  let top = height - tier_top in
  let bottom = top - 3*big_space in
  let left = deck_left in
  let deck1 = List.length state.tier1_deck in
  let deck2 = List.length state.tier2_deck in
  let deck3 = List.length state.tier3_deck in
  (* Draw deck 1 *)
  let () = if deck1 = 0 then begin
    set_color grey;
    fill_rect left bottom deck_width deck_height;
  end else begin
    set_color silver;
    fill_rect left bottom deck_width deck_height;
    set_color black;
    moveto (left + deck_width/2 - 2) (bottom + big_space/2 - 5);
    draw_string (string_of_int deck1); ()
  end in
  (* Draw deck 2 *)
  let () = if deck2 = 0 then begin
    set_color grey;
    fill_rect left (bottom + big_space) deck_width deck_height;
  end else begin
    set_color silver;
    fill_rect left (bottom + big_space) deck_width deck_height;
    set_color black;
    moveto (left + deck_width/2 - 2) (bottom + big_space + big_space/2 - 5);
    draw_string (string_of_int deck2);
  end in
  (* Draw deck 3 *)
  let () = if deck3 = 0 then begin
    set_color grey;
    fill_rect left (bottom + 2*big_space) deck_width deck_height;
  end else begin
    set_color silver;
    fill_rect left (bottom + 2*big_space) deck_width deck_height;
    set_color black;
    moveto (left + deck_width/2 - 2) (bottom + 2*big_space + big_space/2 - 5);
    draw_string (string_of_int deck3);
  end in
  ()

(* Draw one [player] at location [x], [y] *)
let draw_player player color x y =
  set_color color;
  draw_rect x y player_width player_height;
  set_color white;
  moveto (x + 10) (y + player_height - 17);
  draw_string ("points: " ^ string_of_int player.points);
  moveto (x + 150) (y + player_height - 17);
  draw_string (player.name);
  let draw_gems gem color text num =
    let radius = 13 in
    let left = x + 10 + radius in
    let top = (y + player_height - 40) in
    let buffer = 3*radius in
    if gem = 0 then begin (* Don't draw circle if no gems *)
      set_color grey;
      fill_circle (left + num*buffer) top radius;
      () end
    else begin
      set_color color;
      fill_circle (left + num*buffer) top radius;
      moveto (left + num*buffer - 2) (top - 5);
      set_color text;
      draw_string (string_of_int gem);
      () end in
  draw_gems player.gems_held.green green black 0;
  draw_gems player.gems_held.white white black 1;
  draw_gems player.gems_held.blue  blue  white 2;
  draw_gems player.gems_held.black black white 3;
  draw_gems player.gems_held.red   red   black 4;
  draw_gems player.gold            gold  black 5;
  let draw_card card num =
    let c_width = 26 in
    let c_height = 35 in
    let left = x + 10 in
    let bottom = (y + player_height - 100) in
    let buffer = 3*c_width/2 in
    let (color,text) =
        match card.color with
          | Red   -> red, black
          | Blue  -> blue, white
          | Black -> black, white
          | Green -> green, black
          | White -> white, black in
    set_color indigo;
    fill_rect (left + num*buffer) bottom c_width c_height;
    moveto (left + num*buffer + c_width/2 - 2) (bottom + c_height/2 - 5);
    set_color text;
    () in
  let rec draw_all card_lst num =
    match card_lst with
    | []   -> ()
    | h::t -> draw_card h num; draw_all t (num + 1)
  in draw_all player.reserved 0;
  (* Draw Discounts *)
  let draw_discount gem color text num =
    let d_width = 26 in
    let d_height = 26 in
    let left = x + 10 in
    let bottom = (y + player_height - 140) in
    let buffer = 3*d_width/2 in
    if gem = 0 then begin (* Don't draw circle if no gems *)
      set_color grey;
      fill_rect (left + num*buffer) bottom d_width d_height;
      () end
    else begin
      set_color color;
      fill_rect (left + num*buffer) bottom d_width d_height;
      moveto (left + num*buffer + d_width/2 - 2) (bottom + d_height/2 - 5);
      set_color text;
      draw_string (string_of_int gem);
      () end in
  draw_discount player.discounts.green green black 0;
  draw_discount player.discounts.white white black 1;
  draw_discount player.discounts.blue  blue  white 2;
  draw_discount player.discounts.black black white 3;
  draw_discount player.discounts.red   red   black 4;
  ()

(* Draw all players *)
let draw_players state =
 (* draw_player (List.nth state.players 0) red 0 0 *)
  let rec draw_each player num =
    let big_space = player_height + player_buffer in
    match player with
    | []   -> ()
    | h::t -> draw_player h red player_left (height - num*big_space);
        draw_each t (num + 1)
  in draw_each state.players 1

(* [draw_card_info] draws [card] with the options to buy, reserve
 * or cancel the buying process *)
 let draw_card_info card =
  let bx = 125 in
  let tx = 225 in
  let by = 95 in
  let b2y = 125 in
  let offset = 40 in
   draw_card card 10 10 false;
   draw_poly_line [|(0,150);(250,150);(250,0)|];
   moveto (tx-60) 105;
   draw_string ("Buy");
   draw_poly_line [|(bx,by);(bx,b2y);(tx,b2y);(tx,by);(bx,by)|];
   moveto (tx-70) (105-offset);
   draw_string ("Reserve");
   draw_poly_line [|(bx,by-offset);(bx,b2y-offset);(tx,b2y-offset);(tx,by-offset);(bx,by-offset)|];
   moveto (tx-65) (105-offset*2);
   draw_string ("Cancel");
   draw_poly_line [|(bx,by-offset*2);(bx,b2y-offset*2);(tx,b2y-offset*2);(tx,by-offset*2);(bx,by-offset*2)|]


(* Draws a gem in a x y positions *)
let drawsGem x y color =
  let radius = 13 in
  moveto x y;
  let (my_color,txt) = match color with
  | Green -> green,black
  | Blue -> blue,white
  | Red -> red,black
  | Black -> black,white
  | White -> white,black in
  set_color my_color;
  fill_circle x y radius;
  moveto (x-3) (y-5);
  set_color txt;
  draw_string (string_of_int 1)

(* Draws gems in the bottom left corner. *)
let draw_gem_info colorlst =
  let x = 30 in
  let bx = 125 in
  let tx = 225 in
  let by = 95 in
  let b2y = 125 in
  let y = 30 in
  let offset = 40 in
  let index = ref 0 in
  draw_poly_line [|(0,150);(250,150);(250,0)|];
  let rec draw_lst color_list =
    match color_list with
    | [] -> ()
    | h::t ->
      drawsGem x (y+offset*(!index)) h;
      index:=!index+1;
      draw_lst t in
  draw_lst colorlst;
  moveto (tx-60) 105;
  draw_string ("Buy");
  draw_poly_line [|(bx,by);(bx,b2y);(tx,b2y);(tx,by);(bx,by)|];
  moveto (tx-65) (105-offset*2);
  draw_string ("Cancel");
  draw_poly_line [|(bx,by-offset*2);(bx,b2y-offset*2);(tx,b2y-offset*2);(tx,by-offset*2);(bx,by-offset*2)|]

(* Helper function to turn clickable list into color list *)
let rec swap_clickable lst =
match lst with
| [] -> []
| h::t -> match h with
          | Gem(x) -> (int_to_color x)::swap_clickable t
          | _ -> []

(* Displays message for how many turns are left after a player has won *)
let display_final_turns x =
   moveto (width/2 - 250) (height/3);
   draw_string ("Turns Left :");
   draw_string (string_of_int x)

(* Draws the final screen with the list of winners *)
let draw_endgame_display winners=
 create_graph height width;
 let x = width/2 - 25 in
 let y = height/2 + 50 in
 let i = ref 0 in
 moveto (width/2 - 75) (height/2 + 250);
 set_color white;
 draw_string "::WINNERS OF THE GAME ARE::";
 moveto (width/2 - 85) (height/2 + 220);
 draw_string "=============================";
 let rec draw_winners winners offset =
    match winners with
    | [] -> print_string "fdas";
    | h::t -> moveto x (y - offset * (!i));
              i := !i + 1;
              set_color white;
              set_text_size 50;
              draw_string h.name;
              draw_winners t offset
  in draw_winners winners 50

(* Check if user clicked on gem. Returns Some clicked, or None *)
let gem_click mouse_x mouse_y =
  let top = height - top_buffer + gem_radius in
  let diam = 2*gem_radius in
  if mouse_x < left_buffer - gem_radius ||
     mouse_x > left_buffer + gem_radius ||
     mouse_y < top - 6*diam - 5*(gem_buffer - diam) ||
     mouse_y > top
  then None
  else
    (* Check if click is between gems *)
    if (top - mouse_y) mod gem_buffer > diam then None
    else
      let color = (top - mouse_y) / gem_buffer in
      if color = 0      then Some (Gem green)
      else if color = 1 then Some (Gem white)
      else if color = 2 then Some (Gem blue)
      else if color = 3 then Some (Gem black)
      else if color = 4 then Some (Gem red)
      else                   None

(* Check if user clicked on card. Returns Some clicked, or None *)
let card_click mouse_x mouse_y state =
  let top = height - tier_top - card_spacing in
  if mouse_x < tier_left ||
     mouse_x > tier_left + 4*card_width + 3*card_spacing ||
     mouse_y < top - 3*card_height - 2*card_spacing ||
     mouse_y > top
  then None
  else
    (* Check if click is between gems *)
    if (top - mouse_y) mod (card_height + card_spacing) > card_height ||
       (mouse_x - tier_left) mod (card_width + card_spacing) > card_width
    then None
  else
    let tier = 3 - (top - mouse_y)/(card_height + card_spacing) in
    let card = 1 + (mouse_x - tier_left)/(card_width + card_spacing) in
    let tier =
      if tier = 1 then state.tier1
      else if tier = 2 then state.tier2
      else if tier = 3 then state.tier3
      else failwith "code bug" in
    let card = List.nth tier (card - 1) in
    Some (Card (card))

(* Check if user clicked on deck. Returns Some clicked, or None *)
let deck_click mouse_x mouse_y =
  let top = height - tier_top - card_spacing in
  if mouse_x < deck_left ||
     mouse_x > deck_left + deck_width ||
     mouse_y < top - 3*card_height - 2*card_spacing ||
     mouse_y > top
  then None
  else
  (* Check if click is between gems *)
  if (top - mouse_y) mod (card_height + card_spacing) > card_height then None
  else
    let deck = 3 - (top - mouse_y)/(card_height + card_spacing) in
    if deck = 1 then Some Deck1
    else if deck = 2 then Some Deck2
    else if deck = 3 then Some Deck3
    else failwith "code bug"

(* Check if user clicked on a button *)
let button_click mouse_x mouse_y =
  let top = 125 in
  let height = 30 in
  let width = 100 in
  let buffer = 10 in
  let left = 125 in
  if mouse_x < left ||
     mouse_x > left + width ||
     mouse_y < top - 3*height - 2*buffer ||
     mouse_y > top
  then None
  else
    if (top - mouse_y) mod (height + buffer) > height then None
    else
      let button = 3 - (top - mouse_y)/(height + buffer) in
      if button = 1 then Some Cancel
      else if button = 2 then Some Reserve
      else if button = 3 then Some Buy
      else failwith "code bug"

(* Check if user clicked on a reserved card *)
let reserved_click mouse_x mouse_y state =
  let left = 710 in
  let top = 595 in
  let bottom = 560 in
  let width = 26 in
  let buffer = 13 in
  if mouse_x < left ||
     mouse_x > left + 3*width + 2*buffer ||
     mouse_y < bottom ||
     mouse_y > top
  then None
  else
    (* Check if click is between gems *)
    if (mouse_x - left) mod (width + buffer) > width
    then None
  else
    let card = 1 + (mouse_x - left)/(width + buffer) in
    let player1 = List.nth state.players 0 in
    if (List.length player1.reserved < card) then None else
      let card = List.nth player1.reserved (card - 1) in
      Some (Card (card))

(* Check if user clicked on either card or gem. Return clicked *)
let click mouse_x mouse_y state =
  let gem_click = gem_click mouse_x mouse_y in
  let card_click = card_click mouse_x mouse_y state in
  let deck_click = deck_click mouse_x mouse_y in
  let button_click = button_click mouse_x mouse_y in
  let reserved_click = reserved_click mouse_x mouse_y state in
  if gem_click <> None then gem_click
  else if card_click <> None then card_click
  else if deck_click <> None then deck_click
  else if button_click <> None then button_click
  else if reserved_click <> None then reserved_click
  else None

(* Draw the window *)
let draw state =
  create_graph height width;
  draw_gems state;
  draw_cards state;
  draw_decks state;
  draw_players state;
  ()

(* Helper function for valicate_click *)
let validate_gem clickable_lst =
  if List.length clickable_lst = 0 then true
  else if List.length clickable_lst = 1 then
    match List.nth clickable_lst 0 with
    | Card _ -> false
    | _ -> true
  else if List.length clickable_lst = 2 then true
  else false

(* Returns true if new click is a valid move *)
let validate_click clickable_lst new_click =
  if List.length clickable_lst > 3 then failwith "List too long" else
  match new_click with
  | Gem _   -> validate_gem clickable_lst
  | Card _  -> List.length clickable_lst = 0
  | Deck1   -> List.length clickable_lst = 0
  | Deck2   -> List.length clickable_lst = 0
  | Deck3   -> List.length clickable_lst = 0
  | Buy     -> true
  | Reserve -> true
  | Cancel  -> true

(* Update a clickable_list with most recent user click *)
let rec update_click_list clickable_lst new_click =
  if validate_click clickable_lst new_click
    then match new_click with
    | Gem _   -> new_click::clickable_lst
    | Card _  -> new_click::clickable_lst
    | Deck1   -> new_click::clickable_lst
    | Deck2   -> new_click::clickable_lst
    | Deck3   -> new_click::clickable_lst
    | Buy     -> clickable_lst
    | Reserve -> clickable_lst
    | Cancel  -> []
  else clickable_lst

(* Draws the UI for when players need to take thier move*)
let draw_move_UI clickable_list state =
match clickable_list with
| [] ->  draw state
| h::t -> match h with
         | Card (card) -> draw_card_info card
         | Gem(x) -> draw_gem_info (swap_clickable clickable_list)
         | _ -> ()

(* Helper function on whether buy and reserve should be true or false
 * in terms of a valid move *)
 let eval_moves move lst=
 match move with
 | Reserve ->
 if (List.length lst) <1 || (List.length lst) >1  then false
              else
              match (List.hd lst) with
              | Card(x) -> true
              | _ -> false




(* Check if a new move is created *)
let new_move new_click lst =
  match new_click with
  | Gem _   -> false
  | Card _  -> false
  | Deck1   -> true
  | Deck2   -> true
  | Deck3   -> true
  | Buy     -> true
  | Reserve -> eval_moves Reserve lst
  | Cancel  -> false


(* helper for get_move *)
let color_gem gem =
  let c = match gem with | Gem color -> color |_ -> failwith "Only takes gems" in
  if c = red then Red
  else if c = blue then Blue
  else if c = black then Black
  else if c = green then Green
  else if c = white then White
  else failwith "Impossilble"

(* Create a move from clickable_lst and new_click *)
let get_move clickable_lst new_click =
  match new_click with
  | Deck1  -> Top 1
  | Deck2  -> Top 2
  | Deck3  -> Top 3
  | Buy     ->
      begin match List.nth clickable_lst 0 with
      | Gem _  -> begin match clickable_lst with
                  | a::b::c::[] -> Three (color_gem a, Some (color_gem b), Some (color_gem c))
                  | a::b::[]    -> if a=b then Two (color_gem a) else Three (color_gem a, Some(color_gem b), None)
                  | a::[]       -> Three (color_gem a, None, None)
                  | _ -> failwith "This should never happen" end
      | Card c -> Buy c
      | _      -> failwith "This should never happen" end
  | Reserve -> match List.nth clickable_lst 0 with
      | Card c -> Reserve c
      | _      -> failwith "This should never happen"


(* Keep listening to user clicks until you get a valid move to return *)
let rec graphic_play state clickable_lst =
  (* Wait for user click *)
  let player_typ = (List.hd state.players) in
  match player_typ.player_type with
  | Ai(x) -> determine_move state
  | Human ->
    let deets = wait_next_event[Button_down] in
    let mouse_x = deets.mouse_x in
    let mouse_y = deets.mouse_y in
    let new_click = click mouse_x mouse_y state in
    match new_click with
    | None   -> graphic_play state clickable_lst
    | Some c ->
          (* Update list of clicked items *)
          let clickable_lst = update_click_list clickable_lst c in
          (* Draw list of clicked items *)
          draw_move_UI clickable_lst state;
          (* Create a move based on most recent click *)
          if new_move c clickable_lst
          then get_move clickable_lst c
          else graphic_play state clickable_lst

(** Creates a list of the player(s) with the highest point totals*)
let rec calculate_winner_list player_list highest_player acc =
	match player_list with
	| [] -> if List.mem highest_player acc then acc else
				highest_player::acc
	| hd::tl -> if hd.points > highest_player.points then
				let new_acc = [] in
				let new_highest_player = hd in
				calculate_winner_list tl new_highest_player new_acc
				else if hd.points = highest_player.points then
					if List.mem hd acc then let new_acc = hd::acc in
								calculate_winner_list tl highest_player new_acc
					else
						let new_acc = highest_player::(hd::acc) in
						calculate_winner_list tl highest_player new_acc
				else
					calculate_winner_list tl highest_player acc

let rec break_ties highest_player_list highest_player acc =
	match highest_player_list with
	| [] -> if List.mem highest_player acc then acc else
				highest_player::acc
	| hd::tl -> if hd.bought < highest_player.bought then
				let new_acc = [] in
				let new_highest_player = hd in
				break_ties tl new_highest_player new_acc
				else if hd.bought = highest_player.bought then
					if List.mem hd acc then let new_acc = hd::acc in
						break_ties tl highest_player new_acc
					else
						let new_acc = highest_player::(hd::acc) in
						break_ties tl highest_player new_acc
				else
					break_ties tl highest_player acc


(*************************************)
(***********    Testing    ***********)
(*************************************)

let no_gems = {red=0;blue=0;black=0;green=0;white=0;}
let some_gems = {red=1;blue=2;black=3;green=4;white=3;}
let gems1 = {red=3; blue=4; black=0; green=5; white=1;}
let gems2 = {red=2; blue=0; black=0; green=7; white=3;}
let gems3 = {red=0; blue=2; black=3; green=2; white=2;}
let gems4 = {red=2; blue=1; black=1; green=1; white=3;}
let card1 = {color=Black; points=3; gem_cost=gems3;}
let card2 = {color=Green; points=4; gem_cost=gems2;}
let card3 = {color=White; points=2; gem_cost=gems3;}
let card4 = {color=Blue ; points=6; gem_cost=gems4;}
let card5 = {color=Red  ; points=4; gem_cost=gems1;}
let player1 = {name = "dummy"; gems_held=no_gems; discounts=no_gems; reserved=[]; bought=0; points=0; player_type=Human; gold=0}

let the_state = init_state 2 0


(* Take a state, return a move *)
let run state error_msg=
  draw state;
  moveto (width/2 - 250) (height - 30);
  set_color white;
  set_text_size 10;
  draw_string error_msg;
  graphic_play state []

let rec end_game s turns =
  display_final_turns turns;
  match s with
  | (a,b) -> if turns = 0 then
             let no_gems = {red=0;blue=0;black=0;green=0;white=0;} in
             let winnerdummy = {gems_held=no_gems;
                            discounts=no_gems;
                            reserved=[];
                            bought=999;
                            name ="nate smells";
                            points=0;
                            player_type = Human;
                            gold=0} in
             let thewinnerlist = calculate_winner_list a.players winnerdummy [] in
             let final_winnerlist = break_ties thewinnerlist winnerdummy [] in
             display_final_turns turns;
             draw_endgame_display final_winnerlist;
             let deets = wait_next_event[Button_down] in
             ()
             else
             let msg = "turns left : " ^ (string_of_int turns) in
             let themove = run a msg in
             let new_state = play a themove in
             let new_turns = turns - 1 in end_game new_state new_turns


 let rec discard_repl state =
   let last_player = List.nth (state.players) (List.length state.players - 1) in
   if ((calc_players_gems last_player.gems_held) + last_player.gold) <= 10 then state
   else
   match last_player.player_type with
   | Human ->
     let () = draw state in
     let () = moveto (width/2 - 250) (height - 30) in
     let () = set_color white in
     let () = set_text_size 10 in
     let () = draw_string "Please Discard a gem: w,g,(b)lue,b(l)ack,r" in
     let deetz = wait_next_event[Key_pressed] in
     let rec get_disc_color keypress =
       match keypress.key with
       | 'g' -> Green
       | 'w' -> White
       | 'b' -> Blue
       | 'l' -> Black
       | 'r' -> Red
       | _ ->
          let new_deetz = wait_next_event[Key_pressed] in
          get_disc_color new_deetz
       in
    let color = get_disc_color deetz in
    let (new_state,error_msg) = discard state color in
    moveto (width/2 - 250) (height - 30);
    set_color white;
    set_text_size 10;
    draw_string error_msg;
    discard_repl new_state
  | Ai(x) ->
     let color = determine_discard state in
     let (new_state,error_msg) = discard state color in
     moveto (width/2 - 250) (height - 30);
     set_color white;
     set_text_size 10;
     draw_string error_msg;
     discard_repl new_state

 (* a test repl *)
 let rec repl the_state error_msg =
   let discard_state = discard_repl the_state in
   let themove = run discard_state error_msg in
   let new_state = play discard_state themove in
   let n_state = fst(new_state) in
   let last_player = List.nth (discard_state.players) (List.length discard_state.players - 1) in
   if last_player.points >= 15
   then
     let turns_left = n_state.turns_taken mod (List.length n_state.players) in
     end_game new_state turns_left
   else
   match new_state with
   | (a,b) ->
      repl a b; ()

 let () = let start_state = init_state 1 1 in repl start_state ""
