(** The abstract type of values representing monopoly boards. *)
type t

(** [color] is the type representing the possible ANSITerminal colors. *)
type color = | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White 
             | Default	

(** The type of tiles identifiers *)
type tile_id = int

(** The type of card identifiers *)
type card_id = int

(** The type of a property (Baltic Avenue, Reading Railroad...) *)
type property

(** The type of a properties type (Railroad, Utility, Normal) *)
type property_type 

(** The type of a special_tile (Go tile, Chance tile...)*)
type special_tile

(** The type of a specialty_type (Go , Chance...)*)
type specialty_type

(** The action type of a card (Transfer 1, Move 2)  *)
type card_action_type = MoveTo of tile_id | Transfer of int

(* The type of a card *)
type card

(** [from_json json] is an ocaml representation of a monopoly game in JSON. 
    If member "key" is not present, raises [Type_error].
    Preconditions:
    The game must abide by all rules of basic Monopoly. 
    Core game rules CANNOT be adjusted 
    (movement, turn taking, number of tiles, winning, etc). 

    VALID property changes permitted by the JSON are those 
    regarding name, sale price, color, rent, and building costs. 

    VALID community/chance card changes are limited 
    to descriptions, movement, get-out-of-jail-free, collection, payment, 
    and target (individual event or for all players)
*)
val from_json : string -> Yojson.Basic.json -> t

(** [get_file_name game] is the file name of the JSON used to define [game]. *)
val get_file_name : t -> string 

(** [map_id_to_color game] is a list of the id's and colors for all properties. 
    ID's represent the 0-based index of the tiles on the board, 
    starting from the lower right corner and going clockwise. such that
    2 ->3
    ▲   ▼
    1 <-0
    Colors are attached to properties to 
    indicate which groups of properties form monopolies *)
val map_id_to_color : t -> (int * ANSITerminal.style) list

(** [map_id_to_color game] is a list of the ids and names for all properties
    ID's represent the 0-based index of the tiles on the board, 
    starting from the lower right corner and going clockwise. such that
    2 ->3
    ▲   ▼
    1 <-0
    Names are the names that appear on the tiles *) 
val map_id_to_name : t -> (int * string) list

(** [get_tile_ids game] is a list of all tile ids in [game]. *)
val get_tile_ids : t -> tile_id list

(** [get_property_ids game] is a list of all the id's 
    for each of the properties.  *)
val get_property_ids : t -> tile_id list

(** [mortgage_value prop] Getter, the mortgage value of [prop]. *)
val mortgage_value : property -> int

(** [building_cost prop] Getter, the building cost of [prop]. *)
val building_cost : property -> int

(** [id_of_property prop] Getter, the id of [prop]. *)
val id_of_property : property -> int

(** [sale_price prop] Getter, the sale_price of [prop]. *)
val sale_price : property -> int

(** [get_property_by_id id game] is the property object with id [id]. *)
val get_property_by_id : tile_id -> t -> property

(** [rent] Getter, the value at the "rent" key of a property  *)
val rent : property -> int

(** [rent] Getter, the value at the "rent" key of a property  *)
val rent1 : property -> int

(** [rent2] Getter, the value at the "rent2" key of a property  *)
val rent2 : property -> int

(** [rent3] Getter, the value at the "rent3" key of a property  *)
val rent3 : property -> int

(** [rent4] Getter, the value at the "rent4" key of a property  *)
val rent4 : property -> int

(** [hotel_rent] Getter, the value at the "hotel_rent" key of a property  *)
val hotel_rent : property -> int

(** [get_property_set game prop] is a list of the id's for all properties 
    with the same color as [prop]  *)
val get_property_set : t -> property -> tile_id list

(** [is_railroad prop] is true iff prop is railroad. 
    This is detemined by the "property_type" key within the property json *)
val is_railroad : property -> bool

(** [is_normal prop] is true iff prop is normal. 
    This is detemined by the "property_type" key within the property json. 
    Normal Properties are all properties who 
    can have houses or hotels built on them *)
val is_normal : property -> bool

(** [is_utility prop] is true iff prop is utility. 
    This is detemined by the "property_type" key within the property json. *)
val is_utility : property -> bool

(** [is_go tile] is true iff [tile] is Go. 
    This is determined by the "tile_type" property 
    within the "special_tiles" of the JSON *)
val is_go : special_tile -> bool 

(** [is_tax tile] is true iff [tile] is Tax. 
    This is determined by the "tile_type" property 
    within the "special_tiles" of the JSON *)
val is_tax : special_tile -> bool

(** [get_special_tile_by_id id game] is the special_tile object with id [id].*)
val get_special_tile_by_id : tile_id -> t -> special_tile

(** [is_card_tile tile] is true iff [tile] is CardTile. 
    This is determined by the "tile_type" property 
    within the "special_tiles" of the JSON *)
val is_card_tile : special_tile -> bool

(** [is_community_tile tile] is true iff [tile] is ToJail. 
    This is determined by the "tile_type" property 
    within the "special_tiles" of the JSON *)
val is_to_jail : special_tile -> bool

(** [is_special_tile id game] is true iff [id] is a special tile. 
    That is, this tile id exists within the "special_tiles" property 
    of the json *)
val is_special_tile : tile_id -> t -> bool

(** [color_of_id p g] is the color of the property [p] in game [g]. *)
val color_of_id : tile_id -> t -> color

(** [get_card_by_id id game] is the card corresponding to id. *)
val get_card_by_id : card_id -> t -> card

(** [get_card_ids game] is a list of all card ids in [game]. *)
val get_card_ids : t -> card_id list

(** [get_card_action card] is the card action associated with
    [card]. *)
val get_card_action : card -> card_action_type

(** [get_card_description card] is the description associated with
    [card]. *)
val get_card_description : card -> string