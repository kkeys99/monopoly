open Yojson.Basic
open Yojson.Basic.Util
open Game

(** The type representing the outcome for a game. *)
type outcome = Lose | Win

(** The type of a game event. *)
type event = JumpTo | MoveBack | Collect | Pay | Freedom 

(** The type of a chance or community card. *)
type target = Individual | All

(** The type representing jail info. *)
type jail_info = {
  in_jail : bool;
  num_consec_rolls : int;
  turns_in_jail : int
}

(** The type representing a player. *)
type player = {
  name : string;
  current_position : tile_id;
  cash : int;
  in_game : bool;
  jail_data : jail_info 
}

(** The type representing the state of a property *)
type property_state = {
  id : tile_id;
  num_houses : int;
  mortgaged : bool;
}

(** The type representing a state of the game. *)
type t = {
  players : player list;
  owners : (tile_id * string option) list;
  properties : property_state list;
  current_player_index : int;
  num_players : int;
  gameboard : string;
  deck : card_id list;
}

(***** GETTERS *****)
(** [get_players st] is the player list of [st]. *)
let get_players (st : t) : player list =
  st.players

(** [get_player_name p] is the name of [p]. *)
let get_player_name (p : player) : string = 
  p.name

(** [get_player_tile_id p] is the current position of [p]. *)
let get_player_tile_id (p : player) : tile_id = 
  p.current_position

(** [get_player st s] is the [player] with name [n] in state [st]. 
    Raises [Not_found] if [s] is not a name of a player in [st]. *)
let get_player (n : string) (st : t) : player = 
  List.find (fun p -> p.name = n) st.players

(** [get_player_cash p] is the current cash of [p]. *)
let get_player_cash (p : player) : int = 
  p.cash

(** [get_player_in_game p] is true iff [p] is in the game. *)
let get_player_in_game (p : player) : bool = 
  p.in_game

(** [get_player_jail p] is [p]'s jail info. *)
let get_player_jail (p : player) : jail_info = 
  p.jail_data

(** [get_gameboard st] is the gameboard of [st]. *)
let get_gameboard (st : t) : string =
  st.gameboard

(** [get_deck st] is the gameboard of [st]. *)
let get_deck (st : t) : card_id list =
  st.deck

(** [get_owners st] is the owner list of [st]. *)
let get_owners (st : t) : (tile_id * string option) list = 
  st.owners

(** [get_properties st] is the player list of [st]. *)
let get_properties (st : t) : property_state list = 
  st.properties

(** [get_current_player_index st] is the current_player_index of [st]. *)
let get_current_player_index (st : t) : int = 
  st.current_player_index

(** [get_num_players st] is the get_num_players of [st]. *)
let get_num_players (st : t) : int = 
  st.num_players 

(**  [is_in_jail p] is true if a player is in jail else false *)
let is_in_jail (p: player) : bool = p.jail_data.in_jail = true 

(** [get_in_jail j] is true iff [j] is in jail. *)
let get_in_jail (j : jail_info) : bool = 
  j.in_jail 

(** [get_num_consec_roll j] is the num of consecutive double rolls of the
    jail data [j]. *)
let get_num_consec_rolls (j : jail_info) : int = j.num_consec_rolls

(** [turns_in_jail j] is the num of turns in of [j]. *)
let turns_in_jail (j : jail_info) : int = 
  j.turns_in_jail 

(** [get_property_state_id ps] is the id of ps [ps]. *)
let get_property_state_id (ps : property_state) : int = 
  ps.id 

(** [get_num_houses ps] is the num_houses of ps [ps]. *)
let get_num_houses (ps : property_state) : int = 
  ps.num_houses

(** [get_mortgaged ps] is the mortgaged of ps [ps]. *)
let get_mortgaged (ps : property_state) : bool = 
  ps.mortgaged

(** [first_player_index players name] is the index of [name] in [players].
    This function is the same as below, but it is called somewhere where 
    st.players is unavailable. *)
let first_player_index (players : player list) (name : string) : int =
  let rec loop p idx = 
    match p with
    | [] -> failwith "precondition violated in first_player_index"
    | h::t when h.name = name -> idx
    | _::t -> loop t (idx + 1)
  in loop players 0

let get_player_index (st : t) (name : string) : int = 
  first_player_index st.players name

let init_state (game : Game.t) (names : string list) (first : string) : t = 
  let create_players (name_list : string list) = 
    List.map (fun (n : string) : player -> 
        {
          name = n;
          current_position = 0; 
          cash = 1500; 
          in_game = true;
          jail_data = {in_jail = false; num_consec_rolls = 0; turns_in_jail = 0}
        }) name_list
  in
  let create_property_states (g : Game.t) : property_state list = 
    let rec loop tiles acc =
      match tiles with
      | [] -> acc
      | h::t -> loop t ({id = h; num_houses = 0; mortgaged = false} :: acc)
    in loop (get_property_ids g) [] in
  let players = create_players names in 
  {
    players = players;
    owners = List.map (fun (tid : tile_id) : (tile_id * string option) -> 
        (tid, None)) (get_property_ids game);
    properties = create_property_states game;
    current_player_index = first_player_index players first;
    num_players = List.length names;
    gameboard = get_file_name game;
    deck = get_card_ids game;
  }

let draw_card (st : t) (game : Game.t) : (card * t) =
  match st.deck with
  | h::t -> (get_card_by_id h game, { st with deck=t@[h] })
  | [] -> failwith "deck is empty"

(** [current_player st] is the current player in move. *)
let current_player (st : t) : player =
  List.nth st.players st.current_player_index

let current_consec_rolls (st : t) : int = 
  let jail_data = get_player_jail (current_player st) in 
  jail_data.num_consec_rolls

let current_in_jail (st : t) : bool =
  (current_player st).jail_data.in_jail

(** [incr_curr_player_idx st] is state [st] with the current player set to
    the next player. *)
let rec incr_curr_player_idx (st : t) : t = 
  let new_state =
    { st with current_player_index = 
                ((st.current_player_index + 1) mod (st.num_players)) } in
  if (current_player new_state).in_game then new_state else 
    incr_curr_player_idx new_state

(** [update_turns_in_jail st name] is state [st] with the number of
    turns that the player [name] has been in jail incremented by 1.*)
let update_turns_in_jail (st : t) (name : string ) : t = 
  { st with players = List.map 
                (fun p -> if p.name = name then 
                    { 
                      p with 
                      jail_data= {
                        p.jail_data with 
                        turns_in_jail = p.jail_data.turns_in_jail + 1 }
                    } 
                  else p) 
                st.players }

let jail_break (st : t) (name : string ) : t = 
  { st with players = List.map 
                (fun p -> if p.name = name then 
                    { 
                      p with 
                      jail_data= { 
                        turns_in_jail = 0; 
                        in_jail = false;
                        num_consec_rolls = 0 
                      }
                    } 
                  else p) 
                st.players }

let reset_consec_doubles (st : t) : t =
  let curr_name = (current_player st).name in
  { st with players = List.map 
                (fun p -> if p.name = curr_name then 
                    { 
                      p with 
                      jail_data= 
                        { 
                          p.jail_data with
                          num_consec_rolls = 0 
                        }
                    } 
                  else p) 
                st.players }

let change_turn (st : t) : t =
  let new_state = incr_curr_player_idx st in
  let new_current = current_player new_state in
  if new_current.jail_data.in_jail then
    update_turns_in_jail new_state new_current.name
  else 
    new_state

let current_player_name (st : t) : string =
  (current_player st).name

(** [get_player_by_name st name] is the player with [name]. *)
let get_player_by_name (st : t) (name : string) : player = 
  List.find (fun p -> p.name = name) st.players

let current_player_tile (st : t) (game : Game.t) : string = 
  let map = map_id_to_name game in
  List.assoc (current_player st).current_position map

let current_player_tile_id (st : t) : tile_id = 
  (current_player st).current_position 

(** [index_to_name st idx] is the name of the player at [idx]. *)
let index_to_name (st : t) (idx : int) : string = 
  (List.nth st.players idx).name

let move_to (st : t) (pos : tile_id) : t =
  let current = current_player st in 
  { st with players = List.map 
                (fun p : player -> if p = current then 
                    { p with current_position = pos } else p) st.players }

let passed_go (st : t) (roll : int) : bool =
  let current_pos = (current_player st).current_position in
  current_pos > (current_pos + roll) mod 40

let move_forward (st : t) (amt : int) : t = 
  let current_pos = (current_player st).current_position in
  move_to st ((current_pos + amt) mod 40)

let update_cash (st : t) (amt : int) (name : string) : t = 
  { st with players =
              List.map 
                (fun p -> 
                   if p.name = name then 
                     { p with cash = p.cash + amt } 
                   else p) 
                st.players }

let new_state_passed_go (st : t) (roll : int) : t =
  if passed_go st roll then update_cash st 200 (current_player_name st) else st

let get_owner_of_property (st : t) (id : tile_id) : string option =
  try 
    List.assoc id st.owners
  with Not_found -> None

(** [get_property_state_by_id st id] is the property state corresponding
    to [id] in [st].
    Requires: [id] is a valid tile_id. *)
let get_property_state_by_id (st : t) (id : tile_id) : property_state =
  List.find (fun (p : property_state) -> p.id = id) st.properties

let get_num_houses_by_id (st : t) (id : tile_id) : int =
  (get_property_state_by_id st id).num_houses

let tile_ids_owned_by_player (st : t) (name : string) : tile_id list =
  let props = List.filter (fun (_, o) -> 
      match o with 
      | Some owner -> owner = name
      | _ -> false) st.owners
  in List.map (fun (id, _ ) -> id) props

let props_of_owners (st : t) : (int * tile_id) list =
  let rec loop players acc = 
    match players with
    | [] -> acc
    | h::t -> 
      begin
        let index = get_player_index st h.name in
        let tiles = tile_ids_owned_by_player st h.name in
        let rec loop2 
            (tids : tile_id list) 
            (acc2 : (int * Game.tile_id) list) = 
          match tids with
          | [] -> acc2
          | tile::tail -> loop2 tail ((index, tile)::acc2)
        in 
        loop t acc@loop2 tiles acc
      end
  in loop st.players []

(** [properties_owned_by_player st name game] is a list of all properties
    from [game] owned by [name] with [st]. *)
let properties_owned_by_player 
    (st : t) 
    (name : string) 
    (game : Game.t) : property list = 
  let ids = tile_ids_owned_by_player st name in
  List.map (fun id -> get_property_by_id id game) ids

(** [tile_ids_by_player_index st idx] is the list of tile_ids owned by
    by the player at position [idx]. *)
let tile_ids_by_player_index (st : t) (idx : int) : tile_id list = 
  let player = index_to_name st idx in 
  tile_ids_owned_by_player st player

let property_is_buyable (st : t) (id : tile_id) : bool = 
  match get_owner_of_property st id with
  | None -> 
    begin
      (* this will raise an exception if the property isn't in the
         owner list (i.e. it isn't buyable). *)
      try ignore(List.assoc id st.owners); true 
      with Not_found -> false 
    end
  | Some p -> false

let should_pay_rent (st : t) (id : tile_id) : bool =
  try
    match get_owner_of_property st id with
    | None -> false
    | Some owner -> owner <> (current_player_name st)
  with
  | Not_found -> false

(** [buy_helper st game] updates cash count and the owners of [st]. *)
let buy_helper (st : t) (id : tile_id) (game : Game.t) = 
  let prop = get_property_by_id id game in
  let price = sale_price prop in 
  let current_player = current_player st in
  if price <= current_player.cash then
    let new_state = update_cash st (-1 * price) (current_player.name) in
    { new_state with 
      owners = List.map 
          (fun (tid, so) -> 
             if tid = id then 
               (tid, Some (current_player.name))
             else (tid, so)) new_state.owners }
  else st

let property_cost (st : t) (id : tile_id) (game : Game.t) = 
  get_property_by_id id game |> sale_price

let buy_property (st : t) (id : tile_id) (game : Game.t) : t =
  match get_owner_of_property st id with
  | None -> buy_helper st id game
  | Some t -> st

(** [num_owned_helper st name ptype game] is a helper to find the number of 
    properties of a particular type. *)
let num_owned_helper 
    (st : t) 
    (name : string)
    (ptype : Game.property -> bool) 
    (game : Game.t): int = 
  let rec loop prop_list acc =
    match prop_list with
    | [] -> acc
    | h::t -> loop t (if ptype h then acc + 1 else acc)
  in loop (properties_owned_by_player st name game) 0

(** [num_railroads_owned st name] is the number of railroads owned
    by [name] in [st]. *)
let num_railroads_owned (st : t) (name : string) (game : Game.t) : int = 
  num_owned_helper st name is_railroad game

(** [num_utility_owned st name] is the number of utilities owned
    by [name] in [st]. *)
let num_utility_owned (st : t) (name : string) (game : Game.t) : int = 
  num_owned_helper st name is_utility game

let is_monopoly (st : t) (id : tile_id) (game : Game.t) : bool =
  let prop = get_property_by_id id game in
  let pset = get_property_set game prop in
  let rec same_owner (props : tile_id list) (owner : string) : bool =
    match props with
    | [] -> true
    | h::t -> 
      begin
        match get_owner_of_property st h with
        | None -> false
        | Some o -> owner = o && same_owner t owner
      end
  in let first_id = List.nth pset 0 in
  match get_owner_of_property st first_id with 
  | None -> false
  | Some o -> same_owner pset o

(** [rent_cost] is the cost to rent [id] in [game] with state [st]. *)
let normal_rent_cost 
    (st : t) 
    (id : tile_id) 
    (prop : property) 
    (game : Game.t): int = 
  match get_num_houses_by_id st id with
  | 0 -> if (is_monopoly st id game) then 2 * rent prop else rent prop
  | 1 -> rent1 prop
  | 2 -> rent2 prop
  | 3 -> rent3 prop
  | 4 -> rent4 prop
  | 5 -> hotel_rent prop
  | _ -> failwith "precondition failed"

(** [railroad_rent_cost st] is the amount of rent owned the current tile, 
    which is a railroad. 
    Requires: current tile is a railroad, and the owner isn't current. *)
let railroad_rent_cost (st : t) (prop : property) (game : Game.t) : int = 
  match get_owner_of_property st (current_player_tile_id st) with
  | None -> failwith "precondition violated in None of railroad_rent_cost"
  | Some owner -> 
    begin 
      match num_railroads_owned st owner game with
      | 0 -> failwith (owner ^ " doesn't own a railroad.")
      | 1 -> rent prop
      | 2 -> rent1 prop
      | 3 -> rent2 prop
      | 4 -> rent3 prop
      | _ -> failwith "owns more than 4 Railroads in railroad_rent_cost"
    end

(** [utility_rent_cost st prop] is the amount of rent owned on the current 
    tile, which is a utility.
    Requires: current tile is a utility, and the owner isn't current. *)
let utility_rent_cost 
    (st : t) 
    (prop : property) 
    (roll : int) 
    (game : Game.t) : int = 
  match get_owner_of_property st (current_player_tile_id st) with
  | None -> failwith "precondition violated in None of utility_rent_cost"
  | Some owner -> 
    begin 
      match num_utility_owned st owner game with
      | 0 -> failwith (owner ^ " doesn't own a utility.")
      | 1 -> 4 * roll
      | 2 -> 10 * roll
      | _ -> failwith "precondition violated in utility_rent_cost"
    end

(** [pay_rent st prop amt] transfers [amt] because rent had to be paid on
    [prop]. *)
let pay_rent (st : t) (prop : property) (amt : int) (id : tile_id) : t =
  match get_owner_of_property st id with
  | None -> failwith "precondition violated in pay_rent"
  | Some pname -> 
    let new_state = update_cash st amt pname in
    update_cash new_state (amt * -1) (current_player_name st)

let get_rent (st : t) (id : tile_id) (game : Game.t) (roll : int) : int =
  let pstate = id |> get_property_state_by_id st in
  if pstate.mortgaged then 0 else
    let prop = try get_property_by_id id game with _ -> failwith "get_rent" in
    if is_railroad prop then railroad_rent_cost st prop game
    else if is_normal prop then normal_rent_cost st id prop game
    else if is_utility prop then utility_rent_cost st prop roll game
    else failwith "precondition violated in rent"

(** [rent st game] updates the game state so that rent is paid if need be.
    The rent is calculated using [roll] if the tile of the current player is a
    utility. *)
let rent ?(roll = 0) (st : t) (game : Game.t) : t =
  let current_tid = current_player_tile_id st in
  let prop = try get_property_by_id current_tid game 
    with _ -> failwith "rent failed" in
  pay_rent st prop (get_rent st current_tid game roll) current_tid

let toggle_mortgage 
    (st : t) 
    (id : tile_id) 
    (mortgage : bool) 
    (op : int -> int) 
    (game : Game.t) : t = 
  let property = get_property_by_id id game in 
  let price = mortgage_value property in 
  let new_state = {
    st with properties = 
              List.map 
                (fun (p : property_state) -> 
                   if p.id = id then { p with mortgaged = mortgage } else 
                     p) st.properties; } in
  update_cash new_state (op price) (current_player_name new_state)

(** [mortgage_property st id game] is a new state where current
    player has mortgaged [id].
    Requires: current player owns [id] and property not mortgaged. *)
let mortgage_property (st : t) (id : tile_id) (game : Game.t) : t = 
  let pstate = get_property_state_by_id st id in 
  if pstate.num_houses > 0 then st else
    toggle_mortgage st id true (fun p -> p) game

let unmortgage_property (st : t) (id : tile_id) (game : Game.t) : t = 
  toggle_mortgage st id false 
    (fun p -> int_of_float (ceil (float_of_int p) *. -1.1)) game

let toggle_mortgage_pub (st : t) (id : tile_id) (game : Game.t) : t = 
  let pstate = get_property_state_by_id st id in 
  if pstate.mortgaged then unmortgage_property st id game else 
    mortgage_property st id game

(** [transaction_house st id game incr op] is a helper function 
    for buying and selling houses. *)
let transaction_house 
    (st : t)
    (id : tile_id) 
    (game : Game.t) 
    (incr : int -> int)
    (op : int -> int) : t = 
  let property = get_property_by_id id game in
  let price = building_cost property in
  let new_state = {st with 
                   properties = List.map 
                       (fun p -> 
                          if p.id = id then 
                            { p with num_houses = incr p.num_houses } 
                          else p) 
                       st.properties }
  in update_cash new_state (op price) (current_player_name st)

let buy_house (st : t) (id : tile_id) (game : Game.t) : t =
  if is_monopoly st id game then
    transaction_house st id game (fun i -> i + 1) (fun i -> -1 * i)
  else 
    st

let sell_house (st : t) (id : tile_id) (game : Game.t) : t =
  if get_num_houses_by_id st id > 0 then
    transaction_house st id game (fun i -> i - 1) (fun i -> i)
  else 
    st

let player_cash_list (st : t) : (string * int) list = 
  List.map (fun (p : player) -> (p.name, p.cash)) st.players

let is_mortgaged (st : t) (id : tile_id) : bool =
  let pstate = get_property_state_by_id st id in
  pstate.mortgaged

let prop_is_mortgageable (st : t) (id : tile_id) : bool =
  let pstate = get_property_state_by_id st id in
  pstate.num_houses = 0

let get_mortgage_cost (st : t) (id : tile_id) (game : Game.t) : int =
  let base_cost = mortgage_value (get_property_by_id id game) in
  if is_mortgaged st id then 
    floor((float_of_int base_cost *. 1.1 )) |> int_of_float
  else base_cost

let should_current_go_to_jail (st : t) : bool = 
  let curr_player = current_player st in 
  curr_player.jail_data.num_consec_rolls = 3 || 
  curr_player.current_position = 30

let incr_consec_roll (st : t) (name : string) : t = 
  let helper = fun p : player -> if p.name = name then 
      { p with jail_data = {
            p.jail_data with num_consec_rolls = p.jail_data.num_consec_rolls + 1 
          } } else p in
  { st with players = List.map helper st.players }

let send_to_jail (st : t) (name : string) : t =
  let helper = fun p : player -> 
    if p.name = name then 
      { p with jail_data = { 
            p.jail_data with num_consec_rolls = 0; in_jail = true 
          }; current_position = 10 } else p in
  { st with players = List.map helper st.players }

let current_turns_in_jail (st : t) : int =
  let player = current_player st in
  player.jail_data.turns_in_jail

let current_player_mortgagable_props (st : t) : tile_id list = 
  let curr = current_player_name st in 
  let tids = tile_ids_owned_by_player st curr in
  List.filter (fun (id : tile_id) -> get_num_houses_by_id st id = 0) tids 

let current_pos_is_utility (st : t) (game : Game.t) : bool =
  try
    get_property_by_id (current_player_tile_id st) game |> is_utility
  with
  | Not_found -> false

let current_pos_is_tax (st : t) (game : Game.t) : bool =
  try
    get_special_tile_by_id (current_player_tile_id st) game |> is_tax
  with
  | Not_found -> false

let current_pos_is_card (st : t) (game : Game.t) : bool =
  try
    get_special_tile_by_id (current_player_tile_id st) game |> is_card_tile
  with
  | Not_found -> false

let current_player_enough_cash (st : t) (cost : int) : bool =
  ((current_player st).cash) >= cost

(** [curr_player_owns_curr_prop st] is whether the current player owns
    the property they are currently on in state [st]. *)
let curr_player_owns_curr_prop (st : t) : bool = 
  let curr = current_player_tile_id st in
  match get_owner_of_property st curr with
  | None -> false
  | Some owner -> (current_player_name st) = owner

let should_pause (st : t) (game : Game.t) : bool = 
  let curr = current_player_tile_id st in
  curr_player_owns_curr_prop st || is_special_tile curr game

(** [value_of_prop st prop] is the value associated with [prop]. *)
let value_of_prop (st : t) (prop : Game.property) : int = 
  let pstate = get_property_state_by_id st (id_of_property prop) in
  if pstate.mortgaged then 
    0 
  else 
    (pstate.num_houses * building_cost prop) + mortgage_value prop

let current_player_assets (st : t) (game : Game.t) : int = 
  let curr = current_player st in
  let props = properties_owned_by_player st curr.name game in
  List.fold_left 
    (fun acc prop -> acc + value_of_prop st prop) 0 props + curr.cash

let remove_player (st : t) (name : string) (game : Game.t) : t = 
  let player = get_player_by_name st name in
  let props = properties_owned_by_player st player.name game in
  let pids = List.map (fun p -> id_of_property p) props in
  let new_state = 
    { 
      st with 
      properties = List.map (fun p -> 
          if List.mem p.id pids then 
            { p with num_houses = 0; mortgaged = false }
          else p) st.properties
    } in
  let new_state' = 
    { 
      new_state with
      owners = List.map (
          fun (id, so : tile_id * string option) : (tile_id * string option) -> 
            match so with
            | None -> id, None
            | Some s when s = player.name -> id, None
            | Some s -> id, Some s
        ) new_state.owners
    } in
  {
    new_state' with 
    players = List.map (fun p -> 
        if p.name = player.name then 
          { p with in_game = false }
        else p) new_state'.players
  }

(** [translate_jail_info j] is a [jail_info] representation 
    of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let translate_jail_info (j : json) : jail_info = {
  in_jail = j |> member "in_jail" |> to_bool;
  num_consec_rolls = j |> member "num_consec_rolls" |> to_int;
  turns_in_jail = j |> member "turns_in_jail" |> to_int;
}

(** [handle_optional s] converts a string to an optional. *)
let handle_optional (s : string) : string option = 
  if s = " " then None else Some s

(** [translate_player j] is a [player] representation of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let translate_player (j : json) : player = {
  name = j |> member "name" |> to_string;
  current_position = j |> member "current_position" |> to_int;
  cash = j |> member "cash" |> to_int;
  in_game = j |> member "in_game" |> to_bool;
  jail_data = j |> member "jail_info" |> translate_jail_info;
}

(** [translate_owner j] is a [State.owner] representation of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let translate_owner (j : json ) : Game.tile_id * string option = 
  let tile_id = j |> member "tile_id" |> to_int in 
  let owner_name = j |> member "owner_name" |> to_string |> handle_optional in 
  (tile_id, owner_name)

(** [translate_owner j] is a [property_state] representation
    of properties in JSON. 
    If member "key" is not present, raises [Type_error] *)
let translate_property (j : json) : property_state = {
  id = j |> member "id" |> to_int;
  num_houses = j |> member "num_houses" |> to_int;
  mortgaged = j |> member "mortgaged" |> to_bool;
}

(** [translate_deck j] is a card_id representation
    of cards in JSON. 
    If member "id" is not present, raises [Type_error] *)
let translate_deck (j : json) : card_id = 
  j |> member "id" |> to_int

let from_json (j : Yojson.Basic.json) : t = {
  players = j |> member "players" |> to_list |> List.map translate_player;
  owners = j |> member "owners" |> to_list |> List.map translate_owner;
  properties = j |> member "properties" |>to_list|> List.map translate_property;
  current_player_index = j |> member "current_player_index" |> to_int;
  num_players = j |> member "num_players" |> to_int;
  gameboard = j |> member "gameboard" |> to_string;
  deck = j |> member "deck" |> to_list |> List.map translate_deck;
} |> change_turn

let ids_to_num_houses (st : t) : (tile_id * int) list =
  List.map (fun p -> p.id, p.num_houses) st.properties

let current_player_in_game (st : t) : bool =
  (current_player st).in_game

let game_is_over (st : t) : bool =
  let rec loop players acc = 
    if acc > 1 then false else
      match players with 
      | [] -> acc = 1
      | h::t -> loop t (if h.in_game then acc + 1 else acc)
  in loop st.players 0

let properties_with_houses (st : t) (game : Game.t) : tile_id list =
  let tiles = tile_ids_owned_by_player st (current_player_name st) in
  List.filter (
    fun tid -> get_num_houses (get_property_state_by_id st tid) > 0
  ) tiles 

let buildable_properties (st : t) (game : Game.t) : tile_id list = 
  let tiles = tile_ids_owned_by_player st (current_player_name st) in
  List.filter (fun tid -> 
      (not (get_property_state_by_id st tid).mortgaged) 
      && is_monopoly st tid game 
      && get_num_houses (get_property_state_by_id st tid) < 5) 
    tiles 

(** [min l] is the minimum of [l]. *)
let min (l : int list) : int option = 
  if List.length l = 0 then None else 
    let rec loop l min =
      match l with
      | [] -> Some min
      | h::t -> if h < min then loop t h else loop t min
    in loop l (List.nth l 0)

let min_cost_to_build (st : t) (game : Game.t) : int option =
  let not_fully_built = buildable_properties st game in
  let costs = List.map (fun tid -> building_cost (get_property_by_id tid game)) 
      not_fully_built in
  min costs 

let unmortgageable_properties (st : t) : tile_id list = 
  tile_ids_owned_by_player st (current_player_name st) |>
  List.filter (fun tid -> is_mortgaged st tid)

let min_cost_to_unmortgage (st : t) (game : Game.t) : int option = 
  let props = unmortgageable_properties st in
  let costs = List.map (fun tid -> get_mortgage_cost st tid game) props in
  min costs

let mortgageable_properties (st : t) : tile_id list =
  let curr_tiles = tile_ids_owned_by_player st (current_player_name st) in
  let is_mortgageable (st : t) (id : tile_id) : bool =
    let pstate = get_property_state_by_id st id in
    pstate.num_houses = 0 && not pstate.mortgaged 
  in
  List.filter (fun tid -> is_mortgageable st tid) curr_tiles 

let min_expand_cost (st : t) (game : Game.t) : int option =
  match min_cost_to_build st game, min_cost_to_unmortgage st game with
  | None, None -> None
  | Some c, None -> Some c
  | None, Some c -> Some c
  | Some c1, Some c2 -> Some (Pervasives.min c1 c2)