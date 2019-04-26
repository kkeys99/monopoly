open Yojson.Basic
open Yojson.Basic.Util

type color = | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
             | Default	

type tile_id = int

type card_id = int

type property_type = Normal | Railroad | Utility

type specialty_type = Go | Tax | CardTile | Jail | Free | ToJail

type card_action_type = MoveTo of tile_id | Transfer of int

type property = {
  id : tile_id;
  name : string;
  color : color;
  sale_price : int;
  property_type : property_type;
  mortgage_value : int;
  rent : int;
  rent1 : int;
  rent2 : int;
  rent3 : int;
  rent4 : int;
  hotel_rent : int;
  building_cost : int;
} 

(** The type representing a special tile *)
type special_tile = {
  id  : tile_id;
  name : string; 
  tile_type : specialty_type
} 

(** [tile_list] is the type representing the game board's tiles. *)
type tile_list = PTile of property list | STile of special_tile list

(** The type representing a card *)
type card = {
  id : card_id;
  description : string;
  action : card_action_type
}

(** The type representing a game *)
type t = {
  properties : property list;
  special_tiles : special_tile list;
  cards : card list;
  game_name : string
}

(** [ColorNotFound] is an exception when a string representing a color is
    unrecognized. *)
exception ColorNotFound of string

(** [PropertyTypeNotFound] is an exception when a string representing a
    property type is unrecognized. *)
exception PropertyTypeNotFound of string

(** [SpecialtyTypeNotFound] is an exception when a string representing a
    specialty type is unrecognized. *)
exception SpecialtyTypeNotFound of string

(** [CardNotFound] is an exception when a string representing a card is
    unrecognized. *)
exception CardNotFound of string

(** [string_from_json j] is a string of the value of the [property]
    within the [j] json. *)
let string_from_json (j : json) (property:string) : string = 
  member property j |> to_string

(** [color_of_string c] creates a Color from its corresponding string
    raises [ColorNotFound] if the color provided is not possible   *)
let color_of_string (s : string) : color =
  match s |> String.lowercase_ascii with 
  | "black" -> Black
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | "white" -> White
  | "default" -> Default
  | _ -> raise (ColorNotFound ("Color '"^s^"' not found!")) 

(** [ansi_style_of_string s] is the ANSITerminal style for a Color *)
let ansi_style_of_string (c : color) : ANSITerminal.style = 
  match c with 
  | Black -> ANSITerminal.on_black
  | Red -> ANSITerminal.on_red
  | Green -> ANSITerminal.on_green
  | Yellow -> ANSITerminal.on_yellow
  | Blue -> ANSITerminal.on_blue
  | Magenta -> ANSITerminal.on_magenta
  | Cyan -> ANSITerminal.on_cyan
  | White -> ANSITerminal.on_white
  | Default	-> ANSITerminal.on_default

(** [property_type_of_string j] creates a property_type from its corresponding 
    string raises [PropertyTypeNotFound] if the property provided is not 
    possible. *)
let property_type_of_string (s : string) : property_type = 
  match s |> String.lowercase_ascii with 
  | "normal" -> Normal
  | "railroad" -> Railroad
  | "utility" -> Utility
  | _ -> raise (PropertyTypeNotFound ("Property '"^s^"' not found!"))

(** [specialty_type_of_string j] creates a specialty_type from its 
    corresponding string raises [SpecialtyTypeNotFound] if the specialty_type 
    provided is not possible. *)
let specialty_type_of_string (s : string) : specialty_type = 
  match s |> String.lowercase_ascii with 
  | "go" -> Go
  | "tax" -> Tax
  | "cardtile" -> CardTile
  | "jail" -> Jail
  | "free" -> Free
  | _ -> raise (SpecialtyTypeNotFound ("Specialty type '"^s^"' not found!"))

(* * [card_type_of_string s] creates a card_type from its corresponding string
    raises [CardTypeNotFound] if the card_type provided is not possible. *)
let card_type_of_string  (j: json) (s : string) : card_action_type = 
  match s |> String.lowercase_ascii with 
  | "transfer" -> Transfer (j |> member "amount" |> to_int)
  | "move" -> MoveTo (j |> member "tile_id" |> to_int)
  | _ -> raise (CardNotFound ("Card not found!"))

(** [get_property j] is a [property] representation of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let get_property (j : json) : property = {
  id = j |> member "id" |> to_int;
  name = j |> member "name" |> to_string;
  color = j |> member "color"|> to_string |> color_of_string;
  sale_price = j |> member "sale_price" |> to_int;
  property_type = j |> member "property_type" |> to_string 
                    |> property_type_of_string;
  mortgage_value = j |> member "mortgage_value" |> to_int;
  rent = j |> member "rent" |> to_int;
  rent1 = j |> member "rent1" |> to_int;
  rent2 = j |> member "rent2" |> to_int;
  rent3 = j |> member "rent3" |> to_int;
  rent4 = j |> member "rent4" |> to_int;
  hotel_rent = j |> member "hotel_rent" |> to_int;
  building_cost = j |> member "building_cost" |> to_int;
}

(** [get_special_tile j] is a [special_tile] representation of properties in 
    JSON. If member "key" is not present, raises [Type_error] *)
let get_special_tile (j : json) : special_tile = {
  id = j |> member "id" |> to_int;
  name = j |> member "name" |> to_string;
  tile_type = j |> member "tile_type" |> to_string |> specialty_type_of_string;
}

(** [get_card j] is a [card] representation of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let get_card (j : json) : card = {
  id = j |> member "id" |> to_int;
  description = j |> member "description" |> to_string;
  action = j |> member "action" |> to_string |> card_type_of_string j;
}

let from_json (file : string) (json : Yojson.Basic.json) : t = {
  properties = json |> member "properties" |> to_list 
               |> List.map get_property;
  special_tiles = json |> member "special_tiles" |> to_list 
                  |> List.map get_special_tile;
  cards = json |> member "cards" |> to_list 
          |> List.map get_card;
  game_name = file
}

let get_file_name (game : t) : string = game.game_name

let get_tile_ids (game : t) : tile_id list = 
  let rec helper (l : tile_list) (acc : tile_id list) = 
    match l with
    | PTile pl -> 
      begin
        match pl with
        | [] -> acc
        | h::t -> helper (PTile t) (h.id::acc)
      end
    | STile sl ->
      match sl with
      | [] -> acc
      | h::t -> helper (STile t) (h.id::acc)

  in (helper (PTile game.properties) []) |> helper (STile game.special_tiles)

let get_property_ids (game : t) : tile_id list = 
  let rec helper (l : property list) (acc : tile_id list) = 
    match l with
    | [] -> acc
    | h::t -> helper t (h.id::acc)
  in helper game.properties []

let map_id_to_color (game : t) : (int * ANSITerminal.style) list = 
  List.map (fun (p : property) -> 
      (p.id, (ansi_style_of_string p.color))) game.properties 

let map_id_to_name (game : t) : (int * string) list = 
  let props = List.map (fun (p:property) -> (p.id, p.name)) game.properties in
  (0, "Go <------")::(2, "Community Chest")::(4, "Income Tax")::(7, "Chance ?")
  ::(10, "Just Visiting")::(17, "Community Chest")::(20, "Free Parking")::
  (22, "Chance ?")::(30, "To Jail")::(33, "Community Chest")::(36, "Chance ?")
  ::(38, "Luxury Tax")::props

let name (prop : property) : string = prop.name

let mortgage_value (prop : property) : int = prop.mortgage_value

let building_cost (prop : property) : int = prop.building_cost

let id_of_property (prop : property) : tile_id = prop.id

let sale_price (prop : property) : int = prop.sale_price

let get_property_by_id (id : tile_id) (game : t) : property = 
  List.find (fun (p : property) -> p.id = id) game.properties

let get_special_tile_by_id (id : tile_id) (game : t) : special_tile = 
  List.find (fun (s : special_tile) -> s.id = id) game.special_tiles

let rent (prop : property) : int = prop.rent

let rent1 (prop : property) : int = prop.rent1

let rent2 (prop : property) : int = prop.rent2

let rent3 (prop : property) : int = prop.rent3

let rent4 (prop : property) : int = prop.rent4

let hotel_rent (prop : property) : int = prop.hotel_rent

let get_property_set (game : t) (prop : property) : tile_id list =
  let plist = List.filter (fun (p : property) -> p.color = prop.color) 
      game.properties in
  List.map (fun (p : property) -> p.id) plist

let is_railroad (prop : property) : bool = 
  prop.property_type = Railroad

let is_normal (prop : property) : bool = 
  prop.property_type = Normal

let is_utility (prop : property) : bool = 
  prop.property_type = Utility

let is_go (tile : special_tile) : bool = 
  tile.tile_type = Go

let is_tax (tile : special_tile) : bool = 
  tile.tile_type = Tax

let is_card_tile (tile : special_tile) : bool = 
  tile.tile_type = CardTile

let is_to_jail (tile : special_tile) : bool = 
  tile.tile_type = ToJail

let is_special_tile (id : tile_id) (game : t) : bool = 
  List.exists (fun (s : special_tile) -> s.id = id) game.special_tiles

let color_of_id (prop : tile_id) (game : t) : color =
  (get_property_by_id prop game).color

let get_card_by_id (id : card_id) (game : t) : card =
  List.find (fun (c : card) -> c.id = id) game.cards

let get_card_ids (game : t) : card_id list =
  List.map (fun (c : card) -> c.id) game.cards

let get_card_action (card : card) : card_action_type =
  card.action

let get_card_description (card : card) : string =
  card.description