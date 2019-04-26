(** The type representing a state of the monopoly game. *)
type t

(** The type representing a player. *)
type player

(** The type representing a property state. *)
type property_state

(** The type representing a jail_info state. *)
type jail_info

(** [get_current_player_index st] is the current player index of the 
    state [st]. *)
val get_current_player_index : t -> int

(** [get_player_index st name] is the player index that corresponds to the
    player [name]. A player index ranges from 0 to the 
    number of players - 1. *)
val get_player_index : t -> string -> int

(** [get_gameboard st] is the gameboard of [st]. *)
val get_gameboard : t -> string

(** [get_deck st] is the deck of [st]. *)
val get_deck : t -> int list

(** [get_players st] is the player list of [st]. *)
val get_players : t -> player list 

(** [get_player st s] is the [player] with name [s] in state [st]. 
    Raises [Not_found] if [s] is not a name of a player in [st]  *)
val get_player: string -> t -> player

(** [get_owners st] is the owner list of [st]. *)
val get_owners : t -> (Game.tile_id * string option) list 

(** [get_properties st] is the player list of [st]. *)
val get_properties : t -> property_state list 

(** [get_current_player_index st] is the current_player_index of [st]. *)
val get_current_player_index : t -> int

(** [get_num_players st] is the get_num_players of [st]. *)
val get_num_players  : t -> int

(** [get_player_name p] is the name of [p]. *)
val get_player_name : player -> string

(** [get_player_tile_id p] is the current position of [p]. *)
val get_player_tile_id : player -> Game.tile_id

(** [get_player_cash p] is the current cash of [p]. *)
val get_player_cash : player -> int 

(** [get_player_in_game p] is true iff [p] is in the game. *)
val get_player_in_game : player -> bool 

(** [get_player_jail p] is [p]'s jail info. *)
val get_player_jail : player -> jail_info

(** [ids_to_num_houses st] is a map from tile_id to num houses on that tile. *)
val ids_to_num_houses : t -> (Game.tile_id * int) list

(** [get_in_jail j] is true iff [j] is in jail. *)
val get_in_jail : jail_info -> bool

(** [get_num_consec_roll j] is the num of consecutive double rolls of the
    jail data [j]. *)
val get_num_consec_rolls : jail_info -> int

(** [turns_in_jail j] is the num of turns in of [j]. *)
val turns_in_jail : jail_info -> int

(** [get_property_state_id ps] is the id of ps [ps]. *)
val get_property_state_id : property_state -> int

(** [get_num_houses ps] is the num_houses of ps [ps]. *)
val get_num_houses : property_state -> int 

(** [get_mortgaged ps] is the mortgaged of ps [ps]. *)
val get_mortgaged : property_state -> bool 

(** [get_owner_of_property st id] is Some <owner> of tile [id] in state [st] if
    tile [id] has an owner. Otherwise, it is None. *)
val get_owner_of_property : t -> Game.tile_id -> string option

(** [get_num_houses_by_id st id] is the number of houses on [id] in 
    state [st]. *)
val get_num_houses_by_id : t -> Game.tile_id -> int

(** [get_rent st id g roll] is the rental cost of the tile [id] for state [st]
    in game [g]. If the tile is a utility then the rent is [4*roll] if the
    owner of that utility only owns 1 utility. Otherwise, if the owner of that
    utility owns 2 utilities, the rent is [4*roll]. *)
val get_rent : t -> Game.tile_id -> Game.t -> int -> int

(** [get_mortgage_cost st id game] is the cost to present to the KUI.
    It is either the amount you get from mortgaging [id] or the amount you
    have to pay to unmortgage [id]. *)
val get_mortgage_cost : t -> Game.tile_id -> Game.t -> int 

(** [init_state game names first] is the initial state of [game] with players
    [names] and player [first] starting the game. *)
val init_state : Game.t -> string list -> string -> t

(** [change_turn st] is the state [st] advanced to the next player's turn. *)
val change_turn : t -> t

(** [move_to st pos] is the state [st] after the current player moved to
    tile [pos]. Requires: [pos] is a valid Game.tile_id. *)
val move_to : t -> Game.tile_id -> t

(** [move_forward st amt] is the state [st] after the current player moves
    forward [amt] tiles.*)
val move_forward : t -> int -> t

(** [new_state_passed_go st roll] is the state [st] with $200 added to the
    current player's cash if they passed GO by rolling [roll].
    Otherwise, it is [st]. *)
val new_state_passed_go : t -> int -> t

(** [tile_ids_by_player_index st idx] is the list of tile_ids owned by the
    player with index [idx] in state [st].*)
val tile_ids_by_player_index : t -> int -> Game.tile_id list 

(** [property_is_buyable st id] is whether the property at tile [id] is 
    buyable in state [st].*)
val property_is_buyable : t -> Game.tile_id -> bool

(** [should_pay_rent st id] is whether the current player should pay rent
    if they land on tile [id] in state [st].*)
val should_pay_rent : t -> Game.tile_id -> bool

(** [buy_property st id g] is state [st] in game [g] after the current player
    bought the property at tile [id]. *)
val buy_property : t -> Game.tile_id -> Game.t -> t

(** [rent roll st g] is the state [st] of game [g] with any rent paid that is
    needed. *)
val rent : ?roll:int -> t -> Game.t -> t

(** [mortgage_property st id g] is state [st] of game [g] where the current
    player has mortgaged [id].
    Requires: current player owns [id] and property not mortgaged. *)
val mortgage_property : t -> Game.tile_id -> Game.t -> t 

(** [unmortgage_property st id g] is state [st] of game [g] where current player
    has unmortgaged [id]. Requires: current player owns [id] and property
    is mortgaged. *)
val unmortgage_property : t -> Game.tile_id -> Game.t -> t 

(** [toggle_mortgage_pub st id g] is the state [st] of game [g] where the 
    current player has either mortgaged or unmortgaged property [id], depending
    on whether it was previously mortgaged or not. Requires: current player
    owns [id].*)
val toggle_mortgage_pub : t -> Game.tile_id -> Game.t -> t

(** [buy_house st id g] is state [st] of game [g] where current_player
    added a house/hotel to [id]. 
    Requires: current player has a monopoly of [id], and there are still houses
    to buy. *)
val buy_house : t -> Game.tile_id -> Game.t -> t

(** [sell_house st id g] is state [st] of game [g] where current_player
    sold a house/hotel to [id]. 
    Requires: current player has a monopoly of [id], and there are still houses
    to sell. *)
val sell_house : t -> Game.tile_id -> Game.t -> t

(** [player_cash_list st] is a list of all player names and their cash. *)
val player_cash_list : t -> (string * int) list

(** [is_mortgaged st id] is true iff [id] is mortgaged. *)
val is_mortgaged : t -> Game.tile_id -> bool

(** [should_current_go_to_jail st] is true iff current player should go to 
    jail. *)
val should_current_go_to_jail : t -> bool

(** [incr_consec_roll st name] increments the number of consecutive rolls for
    player [name] in state [st]. *)
val incr_consec_roll : t -> string -> t

(** [send_to_jail st n] is state [st] with the player [n] in jail and
    num_consec_rolls is 0.*)
val send_to_jail : t -> string -> t 

(** [current_player_name st] is the name of the current player in state [st].*)
val current_player_name : t -> string

(**  [is_in_jail p] is true if a player is in jail else false *)
val is_in_jail : player ->  bool 

(** [current_player_tile st g] is the name of the current player's tile
    in state [st] of game [g]. *)
val current_player_tile : t -> Game.t -> string

(** [current_player_tile_id st] is the id of the current player's tile in
    state [st]. *)
val current_player_tile_id : t -> Game.tile_id

(** [current_player_mortgagable_props st] is a tile_id list of all properties
    that the current_player owns with 0 houses on them in state [st]. *)
val current_player_mortgagable_props : t -> Game.tile_id list

(** [current_pos_is_utility st g] is true iff the current position of the
    current player for state [st] in game [g] is a utility tile. *)
val current_pos_is_utility : t -> Game.t -> bool 

(** [current_pos_is_tax st g] is true iff the current position of the current
    player for state [st] in game [g] is a tax tile. *)
val current_pos_is_tax : t -> Game.t -> bool

val current_pos_is_card : t -> Game.t -> bool

(** [current_player_enough_cash st cost] is true iff the current player's
    cash is >= [cost] in [st]. *)
val current_player_enough_cash : t -> int -> bool

(** [current_player_assets st name] is the total amount 
    on cash assets for the current player. *)
val current_player_assets : t -> Game.t -> int 

(** [current_consec_rolls st] is the num of consecutive double rolls of the
    current player in state [st]. *)
val current_consec_rolls : t -> int

(** [current_in_jail st] is whether the current player is in jail in state
    [st]. *)
val current_in_jail : t -> bool

(** [current_turns_in_jail st] is the number of turns spent
    in jail for the current player. *)
val current_turns_in_jail : t -> int

(** [current_player_in_game st] is true iff the current player
    is in the game. *)
val current_player_in_game : t -> bool

(** [property_cost st id] is the sale price of [id]. *)
val property_cost : t -> Game.tile_id -> Game.t -> int

(** [should_pause st g] is true iff you should pause before
    moving to the next turn for state [st] of game [g]. *)
val should_pause : t -> Game.t -> bool
(** [remove_player st name game] is a new state where player with [name] is 
    no longer in the game. Gives all properties back to the bank *)
val remove_player : t -> string -> Game.t -> t

(** [update_cash st amt name] changes [name]'s cash by [amt]. *)
val update_cash : t -> int -> string -> t

(** [from_json json] is an ocaml representation of a State.t in JSON. 
    NOTE this increments the player turn, as player save is expected to be 
    the conclusion of a players turn.
    If member "key" is not present, raises [Type_error]. *)
val from_json : Yojson.Basic.json -> t

(** [is_monopoly st id] is true iff [id]'s set is owned by 1 player. *)
val is_monopoly : t -> Game.tile_id -> Game.t -> bool

(** [prop_is_mortgageable st id] is true iff [id] is mortgaged. *)
val prop_is_mortgageable : t -> Game.tile_id -> bool

(** [tile_ids_owned_by_player st name] is a list of all the tile_ids 
    corresponding to the properties that [name] owns. *)
val tile_ids_owned_by_player : t -> string -> Game.tile_id list

(** [jail_break st name] is state [st] with the player [name] out of jail. *)
val jail_break : t -> string -> t 

(** [reset_consec_doubles st] is a new state where the current
    player has no consecutive doubles. This should be called in the
    event that current player didn't roll a double. *)
val reset_consec_doubles : t -> t

(** [props_of_owners st] is a mapping from player indexes to
    tile ids that the player owns. *)
val props_of_owners : t -> (int * Game.tile_id) list


(** [game_is_over st] is true iff 1 player is in the game. *)
val game_is_over : t -> bool

(** [draw_card st game] is the a tuple where the first item is
    the card at the top of the deck and the second is the new deck. *)
val draw_card : t -> Game.t -> (Game.card * t)

(** [properties_with_houses] is a list of all tids with at least one house 
    owned by the current player. *)
val properties_with_houses : t -> Game.t -> Game.tile_id list

(** [min_cost_to_build st game] is the minimum cost of construction on
    a buildable property. *)
val min_cost_to_build : t -> Game.t -> int option

(** [buildable_properties st game] is a tile_id list of all properties that
    can be built upon in [st] of [game].  *)
val buildable_properties : t -> Game.t -> Game.tile_id list

(** [mortgageable_properties st] is a tile_id list of all properties
    that aren't already mortgaged and have no houses owned by current
    player in [st]. *)
val mortgageable_properties : t -> Game.tile_id list

(** [unmortgaged_properties st] is a tile_id list of the current player's
    mortgaged properties. *)
val unmortgageable_properties : t -> Game.tile_id list

(** [min_expand_cost st game] is the minimum cost of either unmortgaging
    something or building a new house. *)
val min_expand_cost : t -> Game.t -> int option

(** [min_cost_to_unmortgage st game] is the minimum cost of
    unmortgaging a mortgaged property for the current player. *)
val min_cost_to_unmortgage : t -> Game.t -> int option