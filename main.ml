open Play
open Graphic

(** This is the main function that calls the necessary functions
to run the game *)
 let () = 
 print_endline "Please enter the number of human_players you would like.\n";
 print_string  "> ";
 let human_players = read_line () in
 let human_int = try int_of_string human_players with
_ -> failwith "Please enter a valid number." in
 print_endline "Please enter the number of Ai players you would like.\n";
 print_string  "> ";
 let ai_players = read_line () in 
 let ai_int = try int_of_string ai_players with
_ -> failwith "Please enter a valid number." in
 let start_state = init_state human_int ai_int in repl start_state ""