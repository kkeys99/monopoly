(** The type that represents any form of input into the game. *)

(** [Empty] Raised when an empty command is parsed. *)
exception Empty

(** [command] is the different kinds of input to the game *)
type command = 
  | Roll
  | RollDouble
  | Menu
  | Mortgage
  | Buy
  | Sell
  | Resign
  | Save
  | Exit  
  | Close
  | Malformed

(** [choice] is a  player's choice when prompted with a yes or no
    question. *)
type choice =
  | Yes
  | No
  | ChoiceMalformed

(** [make_list_of_words s] is a list of words phrase 
    contained in string [s] without any spaces *)
val make_list_of_words : string -> string list

(** [parse c] turns a players input into either a Roll command if the SPACE
    bar is pressed, a Menu command if ESC is pressed or Malformed otherwise. *)
val parse : char -> command

(** [parse_confirm c] is the choice that is parsed from the 
    keyboard input [c]. It is Yes if Y is pressed, No if N is pressed and
    Malformed otherwise. *)
val parse_confirm : char -> choice

(** [parse_menu_option mn] is the command that is parsed from the menu item
    corresponding to [mn]. *)
val parse_menu_option : int -> command

(** [parse_cash_menu_option mn] is the command that is parsed from the menu item
    corresponding to [mn]. *)
val parse_cash_menu_option : int -> command

 