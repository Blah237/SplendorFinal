open OUnit2
open Play


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

(****************************************
 ************ discount records **********
 ****************************************)

let sample_discounts1 = {
  red = 0;
  blue = 0;
  black = 0;
  green = 0;
  white = 0;
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

let sample_player2 = {
  gems_held = sample_gems1;
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
"test1" >:: (fun _ -> assert_equal true (init_state));

]

let _ = run_test_tt_main tests
