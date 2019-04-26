
open State

(** [enclose v] is a string [v] wrapped in double quotes *)
let enclose (v:string) : string = "\"" ^ v ^ "\""

(** [keyify v] is a JSON key from a string, i.e "id" => "id": *)
let keyify (v:string) : string = enclose v ^ ":"

(** [trim_by_n n s] trims a string [s] by [n] characters *)
let trim_by_n n s = 
  String.sub s 0 (String.length s-n)

(** [json_format] formats a [key] and a [value] as a JSON. A [sep] is
    added by default, assuming that the item is not the last property. *)
let json_format ?(sep=",") (key : string) (value:string) : string = 
  key ^ value ^ sep

(** [extract_option] is the string value of a string option. 
None is equivalent to " " *)
let extract_option = function 
  | None -> enclose " "
  | Some s -> enclose s

(** [jail_data_to_json jail] is State.jail_info as a JSON string *)
let jail_data_to_json (jail : jail_info) : string = 
  "{"
  ^ json_format (keyify "in_jail") 
    (jail |> get_in_jail |> string_of_bool) 
  ^ json_format (keyify "num_consec_rolls") 
    (jail |> get_num_consec_rolls|> string_of_int) 
  ^ json_format ~sep:("") (keyify "turns_in_jail") 
    (jail |> turns_in_jail |> string_of_int) 
  ^
  "}"

(** [player_to_json p ] is a State.player as a JSON string *)
let player_to_json (p : player) : string =
  "{"   
  ^ json_format (keyify "name") 
    (p |> get_player_name |> enclose) 
  ^ json_format (keyify "current_position") 
    (p |> get_player_tile_id  |> string_of_int)  
  ^ json_format (keyify "cash") 
    (p |> get_player_cash |> string_of_int)   
  ^ json_format (keyify "in_game") 
    (p |> get_player_in_game |> string_of_bool) 
  ^ json_format ~sep:("") (keyify "jail_info") 
    (p |> get_player_jail |> jail_data_to_json) 
  ^
  "}," 

(** [deck_to_json p] is a State.deck as a JSON string *)
let deck_to_json (p : property_state) : string =
  "{"   
    ^ json_format(keyify "id") (p |> get_property_state_id |> string_of_int) ^
  "},"

(** [property_to_json p] is a State.property_state as a JSON string *)
let property_to_json (p : property_state) : string =
  "{"   
    ^ json_format(keyify "id") 
      (p |> get_property_state_id |> string_of_int) 
    ^ json_format (keyify "num_houses") 
      (p |> get_num_houses |> string_of_int) 
    ^ json_format ~sep:("")(keyify "mortgaged") 
      (p |> get_mortgaged |> string_of_bool) 
    ^
  "},"

(** [convert_list_to_string lst f] takes a list and 
formats it according to [f] as a string JSON list  *)
let convert_list_to_string (lst: 'a list) (f: string -> 'a -> string) : string = 
  "[" 
  ^ 
    (
      List.fold_left (f) "" lst
      |> trim_by_n 1 
    ) 
  ^ 
  "],"

(** [map_player_to_string pLst] is a 
State.player list as a JSON string *)
let map_player_to_string (pLst : player list) : string = 
  convert_list_to_string (pLst) (fun acc i -> acc ^ player_to_json i)

(** [map_property_to_string pLst] is a 
State.property_state list as a JSON string *)
let map_property_to_string (pLst : property_state list) : string = 
  convert_list_to_string (pLst) (fun acc i -> acc ^ property_to_json i)

(** [map_deck_to_string pLst] is a State.deck list as a JSON string
 *)
let map_deck_to_string (dLst : int list) : string = 
  convert_list_to_string (dLst) 
    (fun acc (k) ->
      acc ^ 
      "{" 
        ^ json_format ~sep:"" (keyify "id") (k |> string_of_int) ^ 
      "}," 
    ) |> trim_by_n 1

(** 
   [map_owners_to_string owners] is State.owners as a JSON string. 
   NOTE the 'tile_id' is the key of assoc list, the owner_name is the 
   value of the assoc list. If there is no owner, it is " "
*)
let map_owners_to_string (lst : (int * string option) list) : string =
  convert_list_to_string (lst|>List.rev) 
    (fun acc (k,v) ->
      acc ^ 
      "{" 
        ^ json_format (keyify "tile_id") (k |> string_of_int) 
        ^ json_format ~sep:""(keyify "owner_name") (v |> extract_option) ^
      "}," 
    )

(** [generate_save_string state] produces the 
State.t JSON string to write to a JSON file. *)
let generate_save_string (gamename:string) (state:t) = 
  let players = 
    json_format ~sep:""(keyify "players") 
    (state |> get_players |> map_player_to_string) in 
  let owners = 
    json_format ~sep:""(keyify "owners") 
    (state |> get_owners |> map_owners_to_string) in 
  let properties = 
    json_format ~sep:""(keyify "properties") 
    (state |> get_properties |> map_property_to_string) in 
  let current_player_index = 
    json_format (keyify "current_player_index") 
    (state |> get_current_player_index |> string_of_int) in 
  let num_players = 
    json_format (keyify "num_players") 
    (state |> get_num_players |> string_of_int) in 
  let game = 
    json_format (keyify "gameboard") 
    (state |> get_gameboard |> enclose) in 
  let deck = 
    json_format ~sep:""(keyify "deck") 
    (state |> get_deck |> map_deck_to_string) 
    in  

  "{"^
    players ^
    owners ^ 
    properties ^ 
    current_player_index ^ 
    num_players ^ 
    game ^
    deck ^
  "}"

(**  
   [string_to_file str file] writes a string [str] to a file named [file]
   citation: http://www.codecodex.com/wiki/Save_a_string_to_a_file
*)
let string_to_file file str =
  let oc = open_out file in
  Printf.fprintf oc "%s\n" (str |> Yojson.Basic.prettify);
  close_out oc                


let export_game (state : t) (filename : string) (gamename : string ) : unit = 
  state |> generate_save_string gamename |> string_to_file filename