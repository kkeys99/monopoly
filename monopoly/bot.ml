open State
open Game

type bot_choice = 
  | Continue
  | RollOrMenu of Game.t
  | RaiseCash of bool * int * int
  | BuyProperty of int
  | BuyHouse of ((Game.tile_id * string) list) * Game.t
  | SellHouse of ((Game.tile_id * string) list) * Game.t
  | MainMenu of int * Game.t
  | BMortgage of ((Game.tile_id * string) list) * Game.t * bool * int option

let bot_prefix = '*'

(** [bot_delay] is the forced delay between the AI being prompted with a
    decision and when it makes its decision. This ensures that human users
    have time to perceive the bot's turn. *)
let bot_delay = 0.6

(** [get_monopoly_tiles st g] is a list of the tiles where the current
    player has a monopoly in state [st] of game [g]. *)
let get_monopoly_tiles (state : State.t) (game : Game.t) : Game.tile_id list =
  let tiles_owned = 
    tile_ids_owned_by_player state (current_player_name state) in
  List.fold_left 
    (fun acc id -> if (is_monopoly state id game) then id::acc else acc)
    [] tiles_owned

(** [get_num_houses_owned st] is the number of houses the current player owns
    in state [state]. *)
let get_num_houses_owned (state : State.t) : int = 
  let tiles_owned = 
    tile_ids_owned_by_player state (current_player_name state) in
  List.fold_left (fun acc id -> acc + (get_num_houses_by_id state id)) 
    0 tiles_owned

(** [get_tiles_in_range tiles l u] is a list of the tiles in [tiles] that are
    in the range [l] to [u]. *)
let get_tiles_in_range 
    (tiles : Game.tile_id list) 
    (lower : int) 
    (upper : int) : Game.tile_id list =
  List.fold_left 
    (fun acc id -> if (id >= lower && id <= upper) then id::acc else acc)
    [] tiles

(** [compare_mortgage_costs st g] is a function that is [0] if the 
    mortgage costs of [id1] and [id2] are equal, it is [-1] if the mortgage 
    cost of [id1] is less than [id2] and it is [1] otherwise in state [st]
    of game [g]. *)
let compare_mortgage_costs 
    (state : State.t)
    (game : Game.t) : (Game.tile_id -> Game.tile_id -> int) =
  fun id1 id2 ->
  let mortgage_cost_1 = get_mortgage_cost state id1 game in 
  let mortgage_cost_2 = get_mortgage_cost state id2 game in 
  if mortgage_cost_1 = mortgage_cost_2 then 0
  else if mortgage_cost_1 < mortgage_cost_2 then -1 else 1

(** [bot_roll_menu st g] is whether the bot opens the main menu by pressing
    ESC or rolls the die by pressing SPACE in state [st] of game [g]. *)
let bot_roll_menu 
    (st : State.t) 
    (game : Game.t) : char =
  if current_in_jail st then ' ' else
    let min_cost = min_expand_cost st game in
    match min_cost with
    | Some c -> if current_player_enough_cash st c then '\027' else ' '
    | None -> ' '

(** [bot_raise_cash mp c mn st] is the command the bot uses to navigate the
    menu based on the location of the cursor [mn], whether [mp] it must pay
    $[c] in state [st]. *)
let bot_raise_cash 
    (must_pay : bool) 
    (cost : int) 
    (menu_num : int)
    (state : State.t) : char =
  if current_player_enough_cash state cost && (menu_num <> 3) then 's' else 
  if current_player_enough_cash state cost then  ' '
  else if (not must_pay) && (menu_num <> 3) then 's' 
  else if not must_pay then ' '
  else
    begin 
      let num_mortgageable_props = List.length(mortgageable_properties state) in
      if (num_mortgageable_props > 0) && (menu_num <> 0) then 's'
      else if (num_mortgageable_props > 0) then ' ' (* Mortgage *)
      else if (menu_num <> 1) then 's'
      else ' ' (* Sell House *)
    end

(** [bot_buy_prop cost st] is [y] if the AI decides to buy the property it
    landed on for $[cost] or [n] if it decides not to buy it in state [st]. *)
let bot_buy_prop (cost : int) (state : State.t) : char =
  if current_player_enough_cash state cost then 'y' else 'n'

(** [bot_buy_house km st] is the command the AI uses to buy a house by pressing
    its corresponding key in [km] or exiting the buy house screen by 
    pressing ESC. *)
let bot_buy_house 
    (key_map : (Game.tile_id * string) list) 
    (state : State.t)
    (game : Game.t) : char =
  let buildable_properties = buildable_properties state game in
  if List.length buildable_properties = 0 then '\027' else
    let buyable_side_1 = get_tiles_in_range buildable_properties 0 10 in
    let buyable_side_2 = get_tiles_in_range buildable_properties 10 20 in 
    let buyable_side_3 = get_tiles_in_range buildable_properties 20 30 in 
    let buyable_side_4 = get_tiles_in_range buildable_properties 30 40 in
    let to_buy = 
      if List.length buyable_side_1 > 0 then 
        List.nth buyable_side_1 (Random.int (List.length buyable_side_1))
      else if List.length buyable_side_2 > 0 then 
        List.nth buyable_side_2 (Random.int (List.length buyable_side_2))
      else if List.length buyable_side_3 > 0 then 
        List.nth buyable_side_3 (Random.int (List.length buyable_side_3))
      else List.nth buyable_side_4 (Random.int (List.length buyable_side_4)) in
    let cost = (building_cost (get_property_by_id to_buy game)) in
    if current_player_enough_cash state cost then
      String.get (String.lowercase_ascii (List.assoc to_buy key_map)) 0
    else '\027'

(** [bot_sell_house km st g] is the command the AI uses to sell a house by 
    pressing its corresponding key in [km] or exiting the buy house screen by 
    pressing ESC. *)
let bot_sell_house
    (key_map : (Game.tile_id * string) list)
    (state : State.t)
    (game : Game.t) : char =
  if get_num_houses_owned state > 0 then
    let house_tiles = properties_with_houses state game in 
    let first_sell_id = 
      List.nth house_tiles (Random.int (List.length house_tiles)) in
    String.get (String.lowercase_ascii (List.assoc first_sell_id key_map)) 0
  else 
    '\027'

(** [bot_mortgage km is_mand c st game] is the command the AI uses to mortgage
    or unmortgage a property using the key map [km] in state [st] of game [g].
    The bot is trying to mortgage if [is_mand] and it's trying to unmortgage
    otherwise. If the bot is trying to mortgage [c] is [Some cost] where [cost]
    is the amount of cash the bot is trying to raise, otherwise [c] is [None].*)
let bot_mortgage
    (key_map : (Game.tile_id * string) list)
    (is_mandatory : bool)
    (cost : int option)
    (st : State.t)
    (game : Game.t) : char = 
  let mortgageable_properties = mortgageable_properties st in
  let unmortgageable_properties = unmortgageable_properties st in
  let cash_goal = match cost with 
    | None -> 0
    | Some c -> c in
  if is_mandatory && current_player_enough_cash st cash_goal then '\027'
  else if is_mandatory then
    try
      let cheapest_mortgage = List.nth 
          (List.sort (compare_mortgage_costs st game) mortgageable_properties) 0 
      in String.get 
        (String.lowercase_ascii (List.assoc cheapest_mortgage key_map)) 0
    with
    | Invalid_argument _ | Failure _-> '\027'
  else
    try 
      let cheapest_unmortgage = List.nth 
          (List.sort (compare_mortgage_costs st game) unmortgageable_properties)
          0 in
      String.get 
        (String.lowercase_ascii (List.assoc cheapest_unmortgage key_map)) 0
    with 
    | Invalid_argument _ | Failure _-> '\027'

(** [bot_main_menu mn st g] is the command the bot uses to navigate the main
    menu based on the location of the cursor [mn] and state [st] of game [g].*)
let bot_main_menu 
    (menu_num : int)
    (state : State.t)
    (game : Game.t) : char =
  let can_unmortgage = match min_cost_to_unmortgage state game with 
    | None -> false 
    | Some c -> current_player_enough_cash state c in
  let can_build = match min_cost_to_build state game with 
    | None -> false
    | Some c -> current_player_enough_cash state c in
  if can_unmortgage && (menu_num <> 0) then 's'
  else if can_unmortgage then ' '
  else if can_build && (menu_num <> 1) then 's'
  else if can_build then ' '
  else if (menu_num <> 6) then 's' else
    ' '

let bot_parse (choice : bot_choice) (state : State.t) : char =
  match choice with 
  | Continue -> Unix.sleepf bot_delay; ' '
  | RollOrMenu game -> Unix.sleepf bot_delay; bot_roll_menu state game
  | RaiseCash (must_pay, cost, menu_num) -> 
    Unix.sleepf bot_delay; bot_raise_cash must_pay cost menu_num state
  | BuyProperty cost -> Unix.sleepf bot_delay; bot_buy_prop cost state
  | BuyHouse (key_map, game) -> 
    Unix.sleepf bot_delay; bot_buy_house key_map state game
  | SellHouse (key_map, game) -> 
    Unix.sleepf bot_delay; bot_sell_house key_map state game
  | MainMenu (menu_num, game) -> 
    Unix.sleepf bot_delay; bot_main_menu menu_num state game
  | BMortgage (key_map, game, is_mandatory, cost) -> 
    Unix.sleepf bot_delay; bot_mortgage key_map is_mandatory cost state game