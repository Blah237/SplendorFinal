(* open Card
open Play *)

#require "graphics";;
#use "card.ml";;
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
let height = 680
let width = 920

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

(* [create_graph] creates a blank graph with grey background *)
let create_graph height width =
  close_graph ();
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  set_window_title "Splendor";
  set_color grey;
  fill_rect 0 0 (size_x ()) (size_y ());
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

let state = {
  players = [];
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
  ()


(* (* Check if user clicked on gem. Returns Some color of gem clicked, or None *)
let gem_click mouse_x mouse_y =
  if mouse_x < 10 || mouse_x > 70 then None else
    if mouse_x <

Take status, return string of what happened
let what_happened status =
  let mouse_x = status.mouse_x
  let mouse_y = status.mouse_y
  (* Check if user clicked on gem *)
  let gem_click =
 *)

(*
let play state =
  draw state;
  let rec repl =
    let s = wait_next_event [Button_down] in
 *)