(** The type implementing the saving functionality of the monopoly state  *)

(** 
  [export_game state filename gameboard] exports the State.[state] 
  to file named [filename] using game board [gameboard]

  i.e  export_game state "output.json" "monopoly.json" 
  uses the "monopoly.json" game board and saves the current progress of 
  the game in "output.json"
*)
val export_game : State.t -> string -> string -> unit