open Play
open Card
open Ai
open Graphic

(** This is the main function that calls the necessary functions
to run the game *)
let () =
	let start_state = init_state 1 1 in repl start_state ""