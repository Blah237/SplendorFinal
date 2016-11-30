open OUnit2
open Play
open Card


(****************************************
 ************ Helper Functions **********
 ****************************************)

 (* Creates a sublist from start index b to end index e of the list l
  * i.e sublist 0 4 [1;2;3;4;5;6;7;8] will give [1;2;3;4;5] *)
 let rec sublist b e l =
   match l with
     [] -> failwith "sublist"
   | h :: t ->
      let tail = if e=0 then [] else sublist (b-1) (e-1) t in
      if b>0 then tail else h :: tail



(****************************************
 ************** Gem records *************
 ****************************************)
let sample_gems1 = {
  red = 0;
  blue = 0;
  black = 0;
  green = 0;
  white = 0;
}

let sample_gems2 = {
  red = 0;
  blue = 2;
  black = 0;
  green = 0;
  white = 2;
}

let sample_gems3 = {
  red = 0;
  blue = 2;
  black = 0;
  green = 0;
  white = 0;
}

let sample_gems4 = {
  red = 1;
  blue = 1;
  black = 1;
  green = 0;
  white = 0;
}

let two_players_gems = {
  red = 4;
  blue = 4;
  black = 4;
  green = 4;
  white = 4;
}

let three_players_gems = {
  red = 5;
  blue = 5;
  black = 5;
  green = 5;
  white = 5;
}

let four_players_gems = {
  red = 7;
  blue = 7;
  black = 7;
  green = 7;
  white = 7;
}
(* ############## moves for testing four players ################# *)
(* four player game -2 blue *)
let fpg_2blue = {
  red = 7;
  blue = 5;
  black = 7;
  green = 7;
  white = 7;
  gold = 5;
}
(* four player game -2 blue *)
let fpg_2 = {
  red = 6;
  blue = 4;
  black = 6;
  green = 7;
  white = 7;
  gold = 5;
}
(* ################## moves over for the four player test ######## *)

(****************************************
 ************ discount records **********
 ****************************************)

let sample_discounts1 = {
  red = 0;
  blue = 0;
  black = 0;
  green = 0;
  white = 0;
  gold = 0;
}

(****************************************
 ************* card records *************
 ****************************************)

let sample_card1 = {
  color=Red;
  points=1;
  gem_cost=sample_gems2;
}


(****************************************
 ************ player records ************
 ****************************************)

let sample_player1 = {
  gems_held = sample_gems1;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Human;
}

(* Buys 2 gems *)
let sp1_1 = {
  gems_held = sample_gems3;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Human;
}


let sample_player2 = {
  gems_held = sample_gems1;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Human;
}

(* Buys 3 gems *)
let sp2_1 = {
  gems_held = sample_gems4;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Human;
}

let sample_player3 = {
  gems_held = sample_gems1;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Ai;
}

let sample_player4 = {
  gems_held = sample_gems2;
  discounts = sample_discounts1;
  reserved = [];
  bought = 0;
  points = 0;
  player_type = Ai;
}


(****************************************
 ************ Decks / Nobles ************
 ****************************************)

(* These are the total cards available in the game *)
 let tier1_deck = make_tier_1()
 let tier2_deck = make_tier_2()
 let tier3_deck = make_tier_3()
 (* Total 10 nobles available in the game *)
 let noble_list = make_nobles()

(* Unshuffled, top 4 cards of each deck tier & nobles *)
let t1_unshuffled_top_4 = sublist 0 3 tier1_deck
let t2_unshuffled_top_4 = sublist 0 3 tier2_deck
let t3_unshuffled_top_4 = sublist 0 3 tier3_deck
let noble_unshuffled_top_4 = sublist 0 3 noble_list


(****************************************
 *************** States *****************
 ****************************************)

(* Basic initialize state with the records made above *)
let basic_state = {
  players = [sample_player1;sample_player2;sample_player3;sample_player4];
  current_player = sample_player1;
  tier1_deck = tier1_deck;
  tier2_deck = tier2_deck;
  tier3_deck = tier3_deck;
  tier1 = t1_unshuffled_top_4;
  tier2 = t2_unshuffled_top_4;
  tier3 = t3_unshuffled_top_4;
  nobles = noble_unshuffled_top_4;
  available_gems = four_players_gems;
  gem_piles = 5;
}

(* Player 1 takes 2 gems from basic_state *)
let bs_1 = {
  players = [sp1_1;sample_player2;sample_player3;sample_player4];
  current_player = sample_player2;
  tier1_deck = tier1_deck;
  tier2_deck = tier2_deck;
  tier3_deck = tier3_deck;
  tier1 = t1_unshuffled_top_4;
  tier2 = t2_unshuffled_top_4;
  tier3 = t3_unshuffled_top_4;
  nobles = noble_unshuffled_top_4;
  available_gems = fpg_2blue;
  gem_piles = 5;
}
(* Player 2 takes red,blue,black from bs_1 *)
let bs_2 = {
  players = [sp1_1;sp2_1;sample_player3;sample_player4];
  current_player = sample_player3;
  tier1_deck = tier1_deck;
  tier2_deck = tier2_deck;
  tier3_deck = tier3_deck;
  tier1 = t1_unshuffled_top_4;
  tier2 = t2_unshuffled_top_4;
  tier3 = t3_unshuffled_top_4;
  nobles = noble_unshuffled_top_4;
  available_gems = fpg_2;
  gem_piles = 5;
}


(*#################################################
 **************************************************
 ***************** TEST HARNESS *******************
 **************************************************
 #################################################*)



let tests = "Test Suite for Play" >::: [
(*********************************************************
 *********************************************************
 ********************  INIT_STATE  ***********************
 *********************************************************
 *********************************************************)
"init_state_basic" >:: (fun _ -> assert_equal basic_state (init_state sample_player1 2 2));
"take_two_gems_p1_t1" >:: (fun _ -> assert_equal (Some bs_1) (take_two_gems sample_player1 basic_state Blue (Some bs_1)));
"check_nobles_NONE" >:: (fun _ -> assert_equal None (check_nobles sample_player1 basic_state noble_unshuffled_top_4));
"take_three_p2_t1" >:: (fun _ -> assert_equal bs_2 (take_three_gems sample_player2 bs_1 Red (Some Blue) (Some Black)));

]

let _ = run_test_tt_main tests
