open Card
open Play
open Graphics


(**[init_graphic_state starting_state] initializes the graphical
* interface based on the information from [starting_state]*)
val init_graphic_state : state -> unit


(**[process_click e] is a helper function for [process_event] that 
*	returns the appropriate card if a card has been clicked on and 
*	displays instructions to the player if a button has been clicked on *)
val process_click: event -> card option


(**[process_key e] is a helper function for [process_event] that 
*	returns the corresponding color if a valid key has been pressed
*	that corresponds to an available color gem *)
val process_key: event -> color option

(**[process_event e] processes an event, such as a mouse click, and
* and returns a move if this event is a valid move *)
val process_event : event -> move option

(**[redraw_state] redraws the graphical interface after a move *)
val redraw_state : state -> unit
