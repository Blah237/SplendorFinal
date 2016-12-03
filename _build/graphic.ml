(* open Card
open Play *)

(* #require "graphics";;
#use "card.ml";; *)
open Card
open Graphics

(* colors *)
let red    = rgb 222 27  27
let blue   = rgb 33  149 211
let green  = rgb 59  181 74
let black  = rgb 43  43  43
let white  = rgb 246 246 246
let gold   = rgb 218 165 32
let grey   = rgb 160 160 160
let silver = rgb 192 192 192

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
    set_color color;
    fill_rect (left + num*buffer) bottom c_width c_height;
    moveto (left + num*buffer + c_width/2 - 2) (bottom + c_height/2 - 5);
    set_color text;
    draw_string (string_of_int card.points);
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
  let left = x + 10 + radius in
  let top = (y + player_height - 40) in
  let buffer = 3*radius in
  moveto x y;
  let my_color = match color with
  | Green -> green
  | Blue -> blue
  | Red -> red
  | Black -> black
  | White -> white in
  set_color my_color;
  fill_circle x y radius;
  moveto x y;
  set_color white;
  draw_string (string_of_int 1)

(* Draws gems in the bottom left corner. *)
let draw_gem_info colorlst =
  draw_poly_line [|(0,150);(250,150);(250,0)|]




(* Type for all possible clicks on the board *)
type clickable =
| Green_gem
| White_gem
| Blue_gem
| Black_gem
| Red_gem
| Gold_gem
| Deck1
| Deck2
| Deck3
| Card of int*int  (* Card from deck*order, like deck1*card2 *)
| Buy of clickable list
| Reserve of clickable
| Cancel

(* Draws the UI for when players need to discard gems *)
(* let draw_move_UI clickable_list =
match clickable_list with
| [] -> ()
| h::t -> match h with
         | Card(x,y) -> () (*draw_card_info card*)
         | _ -> () *)



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
      if color = 0      then Some Green_gem
      else if color = 1 then Some White_gem
      else if color = 2 then Some Blue_gem
      else if color = 3 then Some Black_gem
      else if color = 4 then Some Red_gem
      else                   Some Gold_gem

(* Check if user clicked on card. Returns Some clicked, or None *)
let card_click mouse_x mouse_y =
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
    Some (Card (tier, card))

(* Check if user clicked on either card or gem. Return clicked *)
let click mouse_x mouse_y =
  let gem = gem_click mouse_x mouse_y in
  (* If user didn't click on gem, check if they clicked on card,
     else return gem click *)
  if gem = None then card_click mouse_x mouse_y
  else gem


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
let player1 = {gems_held=gems1; discounts=gems2; reserved=[card1;card2]; bought=0; points=5; player_type=Human; gold=5}


let state = {
  players = [player1; player1; player1; player1];
  tier1_deck = [card1; card2; card1; card4; card2; card5];
  tier2_deck = [card1; card1; card1; card2;];
  tier3_deck = [card1; card1; card2;];
  tier1 = [card5; card2; card3; card4];
  tier2 = [card1; card4; card5; card2];
  tier3 = [card4; card2; card1; card3];
  nobles = [gems2; gems3; gems4; gems1; gems2];
  available_gems = some_gems;
  gem_piles = 0;
  turns_taken = 0;
  gold = 6;
}

let draw state =
  create_graph height width;
  draw_gems state;
  draw_cards state;
  draw_decks state;
  draw_players state;
  ()

(* Check if clicking works *)
let rec run state =
  draw state;
  let deets = wait_next_event[Button_down] in
  let mouse_x = deets.mouse_x in
  let mouse_y = deets.mouse_y in
  let result = click mouse_x mouse_y in
  let str =  match result with
    | None    -> "None"
    | Some x  ->
        match x with
        | Green_gem -> "Green"
        | White_gem -> "White"
        | Blue_gem  -> "Blue"
        | Black_gem -> "Black"
        | Red_gem   -> "Red"
        | Gold_gem  -> "Gold"
        | Card (m,n)  -> (string_of_int m) ^ ", " ^ (string_of_int n)
  in print_string str; print_newline ();
  run state
