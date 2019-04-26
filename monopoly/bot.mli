(** The type representing an AI *)

(** CITATION: AI logic inspired by Tim Darling's "surefire" Monopoly strategy:
    http://www.amnesta.net/monopoly/

    The type [bot_choice] represents a decision or action that the AI bot
    has to make:
    - [g] in [RollOrMenu g] is the monopoly game.
    - [b] in [RaiseCash b c mn] specifies whether the bot must raise $[c].
      [mn] is the menu number specifying the location of the menu cursor.
    - [km] in [BuyHouse km g] is the key map for house and [g] is the monopoly
      game. 
    - [km] in [SellHouse km g] is the key map for house and [g] is the monopoly
      game.
    - [g] in [MainMenu mn g] is the monopoly game and [mn] is the menu
      number specifying the location of the menu cursor.
    - [km] in [BMortgage km g mm c] is the key map for house, [g] is the 
      monopoly game and [mm] is whether or not the mortgage prompt was opened 
      for a mandatory mortgage. [c] is [Some cost] if this is a mandatory
      mortgage and [None] if it's not. *)
type bot_choice = 
  | Continue
  | RollOrMenu of Game.t
  | RaiseCash of bool * int * int
  | BuyProperty of int
  | BuyHouse of ((Game.tile_id * string) list) * Game.t
  | SellHouse of ((Game.tile_id * string) list) * Game.t
  | MainMenu of int * Game.t
  | BMortgage of ((Game.tile_id * string) list) * Game.t * bool * int option

(** [bot_prefix] is the prefix for the player name of an AI player. It must
    be a NON-alphanumeric character so that it cannot be entered by a 
    player. *)
val bot_prefix : char

(** [bot_parse c st] is the character input returned by the AI based on
    the choice [c] it has to make in state [st]. *)
val bot_parse : bot_choice -> State.t -> char
