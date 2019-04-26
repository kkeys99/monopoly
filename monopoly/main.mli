(** 
   The main entry point for the monopoly interface.
*)

(** [open_file file] is the JSON contained in [file]. Will prompt for a 
    file repeatedly if [file] doesn't exist *)

val open_file : string -> Yojson.Basic.json