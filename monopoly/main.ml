open Game
open State
open Kui
open Command
open Bot

(** [default_theme] is the default JSON monopoly theme. *)
let default_theme = "monopoly.json"

(** [min_players] is the minimum number of players that this monopoly game
    supports. *)
let min_players = 2

(** [max_players] is the maximum number of players that this monopoly game
    supports.*)
let max_players = 6

(** [open_file file] is the JSON contained in [file]. Will prompt for a 
    file repeatedly if [file] doesn't exist *)
let rec open_file (file : string) : Yojson.Basic.json = 
  try Yojson.Basic.from_file file with 
  | Sys_error e -> raise (Sys_error e)

(** [get_players pn acc] prompts the users for at least [min_players] player
    names until [max_players] player names have been entered or ENTER is 
    pressed. *)
let rec get_players (player_num : int) (acc : string list) : string list =
  if (player_num <= max_players) then
    begin
      print_string ("Player " ^ (string_of_int player_num) ^ ": ");
      try
        match read_line () with
        | "" when player_num <= min_players ->
          ANSITerminal.(print_string [red] ("You must have at least " ^ 
                                            (string_of_int min_players) ^ 
                                            " players, try again.\n"));
          get_players player_num acc
        | "" -> acc
        | "*" -> get_players (player_num+1) 
                   (((Char.escaped bot_prefix) ^ "Bot " ^ 
                     (string_of_int player_num))::acc)
        | x when Str.string_match (Str.regexp "[A-Za-z0-9]+") x 0 -> 
          get_players (player_num+1) (x::acc)
        | _ -> 
          ANSITerminal.(print_string [red] 
                          "\nInvalid player name, try again.\n");
          get_players player_num acc
      with
      | _ -> failwith "Something went wrong! Try CTRL + C to restart."
    end
  else acc

(** [save state filename gamename] saves a state [state] that is being played
    on the board [game_name] as [output_filename]. Example: save st
    "gregs_game.json" "zelda.json" *)
let save 
    (state: State.t) 
    (output_filename: string) 
    (game_name : string) : unit = 
  Save.export_game state output_filename game_name

(** [dice_roll] is a roll of a 6 sided die. *)
let roll () : (int * int) = 
  (Random.int 6+1, Random.int 6+1)

(** [double_roll] is a roll of doubles. *)
let roll_double () : (int * int) =
  let roll_val = Random.int 6+1 in
  (roll_val, roll_val)

(** [player_map plist acc dice_rolls] is an association list with keys
    that are player names and values that are pairs that represent the player's
    intial die rolls. *)
let rec player_map 
    (player_list : string list)
    (acc : (string * (int * int)) list) 
    (dice_rolls : int list) =
  match player_list with
  | [] -> acc
  | name::t -> 
    let f,s = roll () in
    player_map t ((name, (f,s))::acc) ((f+s)::dice_rolls)

(** [map_until_no_ties plist] is an association list of player names and their
    pairs of rolls that contains no ties between players. *)
let rec map_until_no_ties (player_list : string list) =
  let map = player_map player_list [] [] in
  let rolls = List.map (fun (_, (roll1, roll2)) -> (roll1, roll2)) map in
  let rec dup_exist (rlist : (int * int) list) : bool =
    match rlist with
    | [] -> false
    | h::t -> (List.mem h t) || (dup_exist t) in
  if (dup_exist rolls) then map_until_no_ties player_list else map

(** [prompt_menu mn c mp st] is the command that the user selected by pressing 
    SPACE on the menu item identified by [mn] in state [st] where whether
    the player must pay $[c] is [mp]. *)
let rec prompt_cash_menu 
    (menu_num : int) 
    (cost : int) 
    (must_pay : bool)
    (state : State.t) : command = 
  raise_cash_menu menu_num;
  match get1char state (RaiseCash (must_pay, cost, menu_num)) with
  | 'w' -> 
    let num = menu_num - 1 in
    prompt_cash_menu (if num < 0 then 3 else num) cost must_pay state
  | 's' -> 
    prompt_cash_menu ((menu_num + 1) mod 4) cost must_pay state
  | ' ' -> parse_cash_menu_option menu_num
  | _ -> prompt_cash_menu menu_num cost must_pay state

(** [start_game g] starts the game [g] by determining player names and 
    rolling the die to determine the order of turns, returning
    the initial state of the game *)
let rec start_game (game : Game.t) : State.t =
  print_string ("\nPlease enter the names of up to 6 players, or \"*\" for an\n"
                ^ "AI player. Press ENTER to stop entering player names.\n");
  let player_list = get_players 1 [] in
  let players_no_ties = map_until_no_ties player_list in
  let rec helper l (max : string * int) =
    match l,max with
    | [], _ -> max
    | (name, (roll1, roll2))::t, (max_name, max_int)-> 
      if roll1 + roll2 > max_int 
      then helper t (name, roll1 + roll2) 
      else helper t (max_name, max_int) in
  let first,_ = helper players_no_ties ("", 0) in
  let state = State.init_state game (List.rev player_list) first in
  determine_start_player players_no_ties first 
    (get_player_index state first) state game;
  state

(** [handle_not_in_jail st idx name game f,s] is the state where
    you are either sent to jail for rolling a third double or you continue
    your turn as usual. *)
let handle_not_in_jail 
    (st : State.t) 
    (idx : int) 
    (name : string) 
    (game : Game.t)
    (f,s : (int * int)) : State.t =
  let num_consec_rolls = current_consec_rolls st in
  if num_consec_rolls = 3 then
    begin
      animate_dice f s;
      disp_jail_doubles ();
      animate_jail idx (current_player_tile_id st) game st;
      send_to_jail st name
    end
  else 
    begin
      roll_dice name (get_player_index st name) 
        (f,s) (current_player_tile_id st) game;
      move_forward st (f+s)
    end

(** [leave_jail st name (f,s) cm game] is a new state where the 
    [name] has left jail.  *)
let leave_jail 
    (st : State.t)
    (name : string)
    (f,s : (int * int))
    (cash_menu) 
    (game : Game.t): State.t = 
  roll_dice name (get_player_index st name) 
    (f,s) (current_player_tile_id st) game;
  disp_jail_fine ();
  ignore(get1char st Continue);
  if current_player_enough_cash st 50 then
    let paid = update_cash st (-50) name in
    disp_player_info (player_cash_list paid);
    move_forward (jail_break paid name) (f+s)
  else
    begin
      animate_dice f s;
      let out_of_jail = jail_break st name in
      let paid = cash_menu out_of_jail game 50 true name in
      move_forward paid (f+s)
    end

(** [handle_no_double st name fs cm game] is a new state where
    the current player resets his number of consecutive doubles
    and potentially leaves jail, paying a fine.  *)
let handle_no_double 
    (st : State.t) 
    (name : string)
    (f,s : (int * int)) 
    (cash_menu)
    (game : Game.t) : State.t = 
  let no_consec = reset_consec_doubles st in
  if current_in_jail no_consec then
    if current_turns_in_jail no_consec = 3 then
      leave_jail no_consec name (f,s) cash_menu game
    else
      begin
        animate_dice f s;
        disp_stuck_in_jail ();
        no_consec
      end
  else
    begin
      roll_dice name (get_player_index no_consec name) 
        (f,s) (current_player_tile_id no_consec) game;
      let post_go_state = move_forward 
          (new_state_passed_go no_consec (f+s)) (f+s) in
      disp_player_info (player_cash_list post_go_state);
      post_go_state
    end

(** [prompt_roll name s g] is the state after the player [name] rolls the
    die by pressing SPACE in state [s] of game [g]. If the player does not
    press SPACE, wait until they do. *)
let rec prompt_roll 
    (player_name : string)
    (cash_menu : State.t -> Game.t -> int -> bool -> string -> State.t)
    (state : State.t)
    (double : bool)
    (game : Game.t) : State.t = 
  let f,s = if double then roll_double () else roll () in
  let player_index = get_player_index state player_name in
  if f = s then 
    let incremented_doubles = incr_consec_roll state player_name in
    if current_in_jail incremented_doubles then
      let escaped_jail = move_forward 
          (jail_break incremented_doubles player_name) (f+s) in
      roll_dice player_name (get_player_index incremented_doubles player_name) 
        (f,s) (current_player_tile_id incremented_doubles) game;
      escaped_jail
    else
      handle_not_in_jail incremented_doubles player_index player_name game (f,s)
  else
    handle_no_double state player_name (f,s) cash_menu game

(** [prompt_buy st cost] is whether a player decides whether to buy a
    property on the current tile for $[cost] in state [st]. 
    Raises [Malformed] if the player does not press the [Y] or [N] keys 
    to decide. *)
let rec prompt_buy (state : State.t) (cost : int) : bool =
  match Command.parse_confirm (get1char state (BuyProperty cost)) with
  | Yes -> true
  | No -> false
  | ChoiceMalformed -> prompt_buy state cost

(** [prompt_menu m st g] is the command that the user selected by pressing SPACE
    on the menu item identified by [m] in state [st] of game [g]. *)
let rec prompt_menu (menu_num : int) (state : State.t) (game : Game.t) = 
  disp_menu menu_num;
  match get1char state (MainMenu (menu_num, game)) with
  | 'w' -> 
    let num = menu_num - 1 in
    prompt_menu (if num < 0 then 5 else num) state game
  | 's' -> 
    prompt_menu ((menu_num + 1) mod 6) state game
  | ' ' -> parse_menu_option menu_num
  | '\027' -> parse_menu_option 6
  | _ -> prompt_menu menu_num state game

(** [property_keys tids] is an association list with keys that are
    a player's owned property ids [tids] and values that are the 
    keyboard mapping that the player will press to indicate he or she wants to
    mortgage/unmortgage that property. A mortgageable property is one that
    has zero houses on it and is owned by the current player. *)
let property_keys (tids : Game.tile_id list) =
  let rec keys_helper
      (acc : (Game.tile_id * string) list)
      (p_num : int) : (Game.tile_id * string) list =
    try 
      let property_id = List.nth tids p_num in
      let key_mapping =
        if p_num <= 25 then Char.escaped (Char.chr (65 + p_num)) else 
          (string_of_int (p_num - 26)) in 
      keys_helper ((property_id, key_mapping)::acc) (p_num+1)
    with
    | Failure _ -> List.sort compare acc
    | Invalid_argument _ -> failwith "Negative property ID" in 
  keys_helper [] 0

(** [props_info st p_props g] is a list of tuples in which the first value
    is a player's property ids [p_props], the second value is the property's
    mortgage value, the third value is whether the property is currently
    mortgaged or not and the final value is the number of houses on the
    property in state [st] of game [g]. *)
let props_info 
    (state : State.t)
    (player_props : Game.tile_id list) 
    (game : Game.t) : ((Game.tile_id * int * bool * int) list) =
  List.map (fun (prop : Game.tile_id) -> 
      (prop, get_mortgage_cost state prop game,
       is_mortgaged state prop, (get_num_houses_by_id state prop))) 
    (List.sort compare player_props)

(** [get_id_by_key km k] is [Some id] where [id] is the property id associated
    with the key [k] in the keyboard-property map [km] if [k] is a value in 
    [km]. It is [None] otherwise. *)
let get_id_by_key 
    (key_map : (Game.tile_id * string) list) 
    (key : char) : Game.tile_id option =
  let rec get_id_helper 
      (list : (Game.tile_id * string) list) : Game.tile_id option =
    match list with
    | [] -> None
    | (id,k)::t when (Char.escaped key) = (String.lowercase_ascii k) -> Some id
    | _::t -> get_id_helper t in 
  get_id_helper key_map

(** [toggle_prop_mortgage p_info p] is [p_info] with the mortgage status
    toggled for property [p]. *)
let toggle_prop_mortgage 
    (prop_info : (Game.tile_id * int * bool * int) list) 
    (prop : Game.tile_id)
    (new_value : int) =
  let rec toggle_helper l acc =
    match l with
    | [] -> List.sort compare acc 
    | (id, value, is_mortgaged, num_houses)::t when id = prop ->
      toggle_helper t ((id, new_value, not is_mortgaged, num_houses)::acc)
    | info::t -> toggle_helper t (info::acc) in
  toggle_helper prop_info []

(** [change_prop_houses n p_info p] is [p_info] with the number of houses
    for property [p] set to [n]. *)
let change_prop_houses
    (new_num_houses : int)
    (prop_info : (Game.tile_id * int * bool * int) list) 
    (prop : Game.tile_id) =
  let rec house_helper l acc =
    match l with 
    | [] -> List.sort compare acc 
    | (id, value, is_mortgaged, old_num_houses)::t when id = prop ->
      house_helper t ((id, value, is_mortgaged, new_num_houses)::acc)
    | info::t -> house_helper t (info::acc) in
  house_helper prop_info []

(** [prompt_mortgage key_map mand c old_p_info old_st g] is the state after the 
    player has mortgaged or unmortgaged any of his or her properties by pressing
    the key(s) mapped to each of those properties. The players' cash amounts
    are updated each time he or she mortgages or unmortgages a property. The
    player finishes this step by pressing ESC. [c] is [Some cost] where [cost]
    is the amount of cash the player is trying to raise, otherwise [c] is [None]
    to indicate that this mortgage is voluntary. *)
let prompt_mortgage 
    (key_map : (Game.tile_id * string) list) 
    (mandatory_mortgage : bool)
    (cost : int option)
    (old_prop_info : (Game.tile_id * int * bool * int) list) 
    (cash_menu_op : State.t -> Game.t -> int -> bool -> string -> State.t)
    (old_state : State.t)
    (game : Game.t) : State.t =
  let rec prompt_helper 
      (new_prop_info : (Game.tile_id * int * bool * int) list)
      (new_state : State.t) =
    print_player_properties new_prop_info game;
    print_property_prompts key_map game;
    disp_player_info (player_cash_list new_state);
    match get1char new_state 
            (BMortgage(key_map, game, mandatory_mortgage, cost)) with 
    | '\027' -> new_state
    | c when Str.string_match (Str.regexp "[a-z0-9]") (Char.escaped c) 0 ->
      begin
        let prop_id_opt = (get_id_by_key key_map c) in
        match prop_id_opt with 
        | None -> prompt_helper new_prop_info new_state 
        | Some prop_id when prop_is_mortgageable new_state prop_id ->
          begin
            let toggled_state = toggle_mortgage_pub new_state prop_id game in
            let new_m_val = get_mortgage_cost toggled_state prop_id game in
            if is_mortgaged new_state prop_id then
              if current_player_enough_cash new_state 
                  (get_mortgage_cost new_state prop_id game)
              then
                prompt_helper (toggle_prop_mortgage new_prop_info prop_id 
                                 new_m_val) toggled_state
              else
                cash_menu_op new_state game 
                  (get_mortgage_cost new_state prop_id game)
                  mandatory_mortgage
                  (current_player_name new_state)
            else
              prompt_helper (toggle_prop_mortgage new_prop_info prop_id 
                               new_m_val) toggled_state
          end
        | Some prop_id -> 
          disp_invalid_mortgage (); 
          prompt_helper new_prop_info new_state 
      end
    | _ -> prompt_helper new_prop_info new_state in
  prompt_helper old_prop_info old_state

(** [prompt_buy_house km opi os cmo g] is a new state after a player
    bought a house. *)
let prompt_buy_house
    (key_map : (Game.tile_id * string) list) 
    (old_prop_info : (Game.tile_id * int * bool * int) list) 
    (old_state : State.t)
    (cash_menu_op : State.t -> Game.t -> int -> bool -> string -> State.t)
    (game : Game.t) : State.t = 
  let rec prompt_helper
      (new_prop_info : (Game.tile_id * int * bool * int) list)
      (new_state : t) : State.t =
    print_player_properties new_prop_info game;
    print_property_prompts key_map game;
    disp_player_info (player_cash_list new_state);
    match get1char new_state (BuyHouse (key_map, game)) with
    | '\027' -> new_state
    | c when Str.string_match (Str.regexp "[a-z0-9]") (Char.escaped c) 0 -> 
      begin
        let prop_id_opt = (get_id_by_key key_map c) in
        match prop_id_opt with
        | None -> prompt_helper new_prop_info new_state
        | Some prop_id when not (is_monopoly new_state prop_id game) ->
          disp_monopoly_message ();
          prompt_helper new_prop_info new_state
        | Some prop_id when (get_num_houses_by_id new_state prop_id) = 5 ->
          disp_too_many_houses ();
          prompt_helper new_prop_info new_state
        | Some prop_id when (is_railroad (get_property_by_id prop_id game)) ->
          disp_no_railroad_house ();
          prompt_helper new_prop_info new_state
        | Some prop_id when current_player_enough_cash new_state
              (building_cost (get_property_by_id prop_id game)) -> 
          disp_buy_house prop_id 
            (get_num_houses_by_id new_state prop_id + 1) game;
          prompt_helper (change_prop_houses 
                           ((get_num_houses_by_id new_state prop_id)+1) 
                           new_prop_info prop_id)
            (buy_house new_state prop_id game)
        | Some prop_id -> 
          cash_menu_op new_state game 
            (building_cost (get_property_by_id prop_id game))
            false
            (current_player_name new_state)
      end
    | _ -> prompt_helper new_prop_info new_state
  in prompt_helper old_prop_info old_state

(** [prompt_sell_house km opi os cmo game] is a new state after a player
    sold a house. *)
let prompt_sell_house 
    (key_map : (Game.tile_id * string) list) 
    (old_prop_info : (Game.tile_id * int * bool * int) list) 
    (old_state : State.t)
    (cash_menu_op : State.t -> Game.t -> int -> bool -> string -> State.t)
    (game : Game.t) : State.t = 
  let rec prompt_helper
      (new_prop_info : (Game.tile_id * int * bool * int) list)
      (new_state : t) : State.t =
    print_player_properties new_prop_info game;
    print_property_prompts key_map game;
    disp_player_info (player_cash_list new_state);
    match get1char new_state (SellHouse (key_map, game)) with
    | '\027' -> new_state
    | c when Str.string_match (Str.regexp "[a-z0-9]") (Char.escaped c) 0 -> 
      begin
        let prop_id_opt = (get_id_by_key key_map c) in
        match prop_id_opt with
        | None -> prompt_helper new_prop_info new_state
        | Some prop_id when (get_num_houses_by_id new_state prop_id) = 0 ->
          disp_no_houses ();
          prompt_helper new_prop_info new_state
        | Some prop_id -> 
          disp_buy_house prop_id 
            (get_num_houses_by_id new_state prop_id - 1) game;
          prompt_helper (change_prop_houses 
                           ((get_num_houses_by_id new_state prop_id)-1) 
                           new_prop_info prop_id)
            (sell_house new_state prop_id game)
      end
    | _ -> prompt_helper new_prop_info new_state
  in prompt_helper old_prop_info old_state

(** [sell_house st p_info map cmo game] adjusts KUI and prompts
    sale of houses. *)
let sell_house
    (st : State.t)
    (p_info : (int * int * bool * int) list)
    (map : (int * string) list)
    (cash_menu_op : State.t -> Game.t -> int -> bool -> string -> State.t)
    (game : Game.t) : State.t =
  clear_menu (); 
  print_house_message ();
  print_player_properties p_info game;
  print_property_prompts map game;
  prompt_sell_house map p_info st cash_menu_op game

(** [remove_player_from_game st name game] removes players from [st] and updates
    the KUI to reflect those changes. *)
let remove_player_from_game 
    (st : State.t) 
    (name : string) 
    (game : Game.t) : State.t =
  let idx = get_current_player_index st in
  redistribute_property (idx, (tile_ids_owned_by_player st name)) st game;
  remove_player st name game

(** [close_cash_menu st mp cm c n g] is a helper function that closes the 
    cash menu or prompts it to open again if you don't have enough cash.  *)
let close_cash_menu 
    (st : State.t )
    (must_pay : bool)
    (cash_menu)
    (cost : int)
    (name : string)
    (game : Game.t) : State.t =
  if must_pay && not (current_player_enough_cash st cost) then 
    cash_menu st game cost must_pay name 
  else if must_pay then
    begin
      clear_cash_menu ();
      let paid = update_cash st (-cost) name in
      disp_player_info (player_cash_list paid);
      forced_payment cost paid;
      paid
    end
  else 
    begin
      clear_cash_menu (); st
    end

(** [cash_menu st game cost must_pay player_name] prompts the 
    current player for a cash menu if they don't have enough money
    for whatever they're trying to pay for. [must_pay] tells you if the 
    exchange is manadatory. *)
let rec cash_menu
    (st : State.t) 
    (game : Game.t) 
    (cost : int) 
    (must_pay : bool)
    (player_name : string) : State.t = 
  if current_player_assets st game < cost && must_pay then
    remove_player_from_game st player_name game
  else
    let player_index = get_current_player_index st in
    let properties_kui_info = props_info st 
        (tile_ids_by_player_index st player_index) game in
    let key_map = property_keys 
        (List.sort compare (tile_ids_owned_by_player st player_name)) in
    match prompt_cash_menu 0 cost must_pay st with
    | Mortgage -> 
      clear_menu (); 
      mortgage_message ();
      print_player_properties properties_kui_info game;
      print_property_prompts key_map game;
      cash_menu (prompt_mortgage key_map must_pay 
                   (Some cost) properties_kui_info cash_menu st game) 
        game cost must_pay player_name
    | Sell -> 
      let post_sell_house_state = 
        sell_house st properties_kui_info key_map cash_menu game in
      cash_menu post_sell_house_state game cost must_pay player_name
    | Resign -> remove_player_from_game st player_name game
    | Close -> close_cash_menu st must_pay cash_menu cost player_name game
    | Malformed -> failwith "malformed command in cash_menu"
    | _ -> failwith "invalid command"

(** [next_turn otile cont g old_st] advances the game [g] to the next turn using 
    the main function [cont] in which the former player was on tile [otile] and
    the end of the previous turn had state [old_st]. *)
let next_turn 
    (old_tile : tile_id)
    (continue : Game.t -> State.t -> unit)
    (game : Game.t)
    (old_state : State.t) : unit =
  if (current_consec_rolls old_state > 0) && 
     (current_player_in_game old_state) then 
    begin
      disp_rolled_double old_state;
      continue game old_state 
    end
  else
    begin
      let next_turn_state = change_turn old_state in
      let new_tile = current_player_tile_id next_turn_state in
      let name = current_player_name next_turn_state in
      let new_player = get_player_index next_turn_state name in
      disp_change_turn old_tile new_player new_tile game;
      continue game next_turn_state
    end

(** [open_cash_menu st tile cost must_pay name cont game] opens the cash menu 
    for a player and goes to the next turn after [name]'s turn is over. *)
let open_cash_menu 
    (st : t) 
    (tile : Game.tile_id)
    (cost : int)
    (must_pay : bool)
    (name : string)
    (continue : Game.t -> State.t -> unit)
    (game : Game.t) : unit =
  let new_state = cash_menu st game cost must_pay name in
  next_turn tile continue game new_state

(** [handle_card st idx name t g c] continues the game given that
    the current player landed on a card tile. *)
let handle_card 
    (st : State.t)
    (idx : int)
    (name : string)
    (tile : tile_id)
    (game : Game.t)
    (continue) : unit = 
  let card, drawn_state = draw_card st game in
  card_message (get_card_description card) drawn_state;
  match get_card_action card with
  | MoveTo tile_id ->
    if tile_id = 10 then
      begin
        animate_jail idx tile game drawn_state;
        disp_go_to_jail ();
        let jail = send_to_jail drawn_state name in 
        next_turn tile_id continue game jail
      end
    else if tile_id < tile then
      begin
        animate_card_move tile tile_id idx game;
        let passed_go = update_cash drawn_state 200 
            (current_player_name drawn_state) in
        let moved = move_to passed_go tile_id in
        next_turn tile_id continue game moved
      end
    else
      begin
        animate_card_move tile tile_id idx game;
        let moved = move_to drawn_state tile_id in
        next_turn tile_id continue game moved
      end
  | Transfer amt -> 
    begin
      if current_player_enough_cash drawn_state (-amt) then
        let post_transfer = update_cash drawn_state amt name in
        disp_player_info (player_cash_list post_transfer);
        next_turn tile continue game post_transfer
      else
        open_cash_menu drawn_state tile (-amt) true name continue game
    end

(** [handle_buyable_prop st game tile name idx continue] prompts the player
    to buy the current tile_id. If the player cannot afford it, the cash menu 
    opens and then the game continues. *)
let handle_buyable_prop 
    (st : State.t)
    (game : Game.t)
    (tile : tile_id)
    (name : string)
    (idx : int)
    (continue) : unit = 
  disp_buy_message tile game;
  let price = property_cost st tile game in
  if prompt_buy st price then
    if current_player_enough_cash st price then
      let post_buy = State.buy_property st tile game in 
      buy_property tile (get_player_index post_buy name) game;
      disp_player_info (player_cash_list post_buy);
      let new_props_kui_info = props_info post_buy 
          (tile_ids_by_player_index post_buy idx) game in
      print_player_properties new_props_kui_info game;
      confirm_buy post_buy;
      next_turn tile continue game post_buy
    else
      open_cash_menu st tile price false name continue game
  else
    begin
      deny_buy st;
      next_turn tile continue game st
    end

(** [handle_pay_rent st game tile name continue] pays rent from the current
    player and potentially opens the cash menu and continues the game. *)
let handle_pay_rent 
    (st : State.t)
    (game : Game.t)
    (tile : tile_id)
    (name : string)
    (continue) : unit = 
  let f,s = roll () in
  let is_utility = current_pos_is_utility st game in
  let post_rent_state = rent ~roll:(f+s) st game in
  let rent = get_rent st tile game (f+s) in
  if current_player_enough_cash st rent then
    let tile_owner =
      match get_owner_of_property st tile with 
      | Some x -> x 
      | _ -> failwith "Prec violated in continue_game tile_owner" in
    disp_rent_message ~is_utility:is_utility ~roll:(f,s) 
      (current_player_tile st game) 
      tile_owner rent post_rent_state;
    disp_player_info (player_cash_list post_rent_state);
    next_turn tile continue game post_rent_state
  else 
    open_cash_menu st tile rent true name continue game

(** [handle_current_tile tile name player_idx cont g st] handles the action
    associated with the [tile] that [player_idx] is currently on in state [st]
    of game [g]. The game progresses with the main game loop [cont]. *)
let rec handle_current_tile 
    (tile : Game.tile_id)
    (name : string)
    (player_index : int)
    (continue : Game.t -> State.t -> unit)
    (game : Game.t)
    (post_roll_menu_state : State.t) : unit =
  if not (current_player_in_game post_roll_menu_state) then
    next_turn (current_player_tile_id post_roll_menu_state) 
      continue game post_roll_menu_state
  else if current_pos_is_card post_roll_menu_state game then
    handle_card post_roll_menu_state player_index name tile game continue
  else if should_current_go_to_jail post_roll_menu_state then
    begin
      disp_go_to_jail ();
      animate_jail player_index tile game post_roll_menu_state;
      next_turn 10 continue game (send_to_jail post_roll_menu_state name)
    end
  else if property_is_buyable post_roll_menu_state tile then
    handle_buyable_prop post_roll_menu_state game tile name player_index continue
  else if should_pay_rent post_roll_menu_state tile then
    handle_pay_rent post_roll_menu_state game tile name continue
  else if current_pos_is_tax post_roll_menu_state game then
    if current_player_enough_cash post_roll_menu_state 200 then
      begin 
        disp_tax_message 200 post_roll_menu_state;
        let taxed_state = update_cash post_roll_menu_state (-200) name in
        disp_player_info (player_cash_list taxed_state);
        next_turn tile continue game taxed_state
      end
    else
      open_cash_menu post_roll_menu_state tile 200 true name continue game
  else if should_pause post_roll_menu_state game then 
    begin
      pause_message post_roll_menu_state;
      next_turn tile continue game post_roll_menu_state
    end
  else next_turn tile continue game post_roll_menu_state

(** [continue_game g s] executes an entire user's turn and advances
    state [s] in game [g] to the resulting state. *)
let rec continue_game (game : Game.t) (state : State.t) : unit = 
  if game_is_over state then
    win_message state
  else
    begin 
      let name = current_player_name state in
      let current_tile = current_player_tile state game in
      let player_index = get_player_index state name in
      let player_properties = tile_ids_by_player_index state player_index in
      let old_props_kui_info = props_info state player_properties game in
      let post_roll_menu_state =
        let rec pre_roll_helper 
            (name : string)
            (new_state : State.t)
            (new_props_info : (Game.tile_id * int * bool * int) list)
            (game : Game.t) : State.t = 
          disp_turn name player_index current_tile;
          print_player_properties new_props_info game;
          let key_map = property_keys 
              (List.sort compare (tile_ids_owned_by_player new_state name)) in
          match Command.parse (get1char new_state (RollOrMenu game)) with 
          | Roll -> prompt_roll name cash_menu new_state false game
          | RollDouble -> prompt_roll name cash_menu new_state true game
          | Menu when not (current_in_jail new_state) -> 
            begin 
              match prompt_menu 0 new_state game with 
              | Mortgage ->
                clear_menu ();
                mortgage_message ();
                print_player_properties new_props_info game;
                print_property_prompts key_map game;
                let post_mortgage_state = prompt_mortgage key_map false 
                    None new_props_info cash_menu new_state game in
                let post_mortgage_kui_info = props_info post_mortgage_state 
                    player_properties game in
                pre_roll_helper name post_mortgage_state 
                  post_mortgage_kui_info game
              | Buy ->
                clear_menu (); 
                print_house_message ();
                print_player_properties new_props_info game;
                print_property_prompts key_map game;
                let post_buy_house_state = prompt_buy_house key_map 
                    new_props_info new_state cash_menu game in
                let post_buy_house_kui_info = props_info post_buy_house_state 
                    player_properties game in 
                pre_roll_helper name post_buy_house_state 
                  post_buy_house_kui_info game
              | Sell ->
                let post_sell_house_state = 
                  sell_house new_state new_props_info key_map cash_menu game in
                let post_sell_house_kui_info = props_info post_sell_house_state
                    player_properties game in
                pre_roll_helper name post_sell_house_state
                  post_sell_house_kui_info game
              | Save ->
                clear_menu();
                let timestamp = string_of_int (int_of_float (Unix.time ())) in
                let output_filename = ("monopoly" ^ timestamp ^ ".json") in
                save new_state output_filename (get_file_name game);
                pre_roll_helper name new_state new_props_info game
              | Resign ->
                clear_menu();
                remove_player_from_game new_state name game
              | Close -> 
                clear_menu ();
                pre_roll_helper name new_state new_props_info game
              | Exit -> 
                ANSITerminal.erase ANSITerminal.Screen;
                exit 0
              | _ -> prompt_roll name cash_menu new_state false game
            end
          | _ -> pre_roll_helper name new_state new_props_info game in
        pre_roll_helper name state old_props_kui_info game in
      let tile = (current_player_tile_id post_roll_menu_state) in
      handle_current_tile tile name player_index continue_game 
        game post_roll_menu_state
    end

(** [play_new_game] starts a new monopoly game with a JSON file theme
    specified by the user. *)
let rec play_new_game (first : bool) : unit =
  if first then
    begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.set_cursor 1 1;
      ANSITerminal.(print_string [yellow] 
                      ("Please enter the name of the monopoly theme JSON file\n"
                       ^ "you want to load or press ENTER for the default\n" ^
                       "Monopoly theme. \n(NOTE: you may have to change " ^
                       "the font/window size for the GUI to render " ^
                       "properly)\n";));
      print_string "> ";
    end
  else
    begin
      ANSITerminal.(print_string [red] "Invalid JSON file name. Try again.\n");
      print_string "> ";
    end;
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    try let game = 
          (if (file_name = "") then 
             open_file default_theme |> Game.from_json default_theme
           else 
             open_file file_name |> Game.from_json file_name) in 
      continue_game game (start_game game)
    with Sys_error e -> play_new_game false

(** [load_game] prompts the user for the JSON file containing the saved state
    of a monopoly game and resumes it. *)
let rec load_game (first : bool) : unit =
  if first then
    begin
      ANSITerminal.erase ANSITerminal.Screen;
      ANSITerminal.set_cursor 1 1;
      ANSITerminal.(print_string [yellow] 
                      ("Please enter the name of the saved game file you want\n"
                       ^ "to load.\n";));    
      print_string "> ";
    end 
  else    
    begin
      ANSITerminal.(print_string [red] "Invalid JSON file name. Try again.\n");
      print_string "> ";
    end;
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    try
      let loaded_state = State.from_json (open_file file_name) in
      let gameboard_json = get_gameboard loaded_state in
      try let game = Game.from_json gameboard_json 
              (Yojson.Basic.from_file gameboard_json) in
        print_board_from_file (props_of_owners loaded_state) 
          (get_current_player_index loaded_state) 
          (current_player_tile_id loaded_state) (player_cash_list loaded_state) 
          (ids_to_num_houses loaded_state) game;
        continue_game game loaded_state
      with 
      | Sys_error e -> load_game false
    with
    | Sys_error e -> load_game false

(** [main ()] prompts the player to either start a new game or load
    a previous one from a JSON file and then starts it. *)
let main () = 
  let rec load_or_save () =
    ANSITerminal.resize 200 200;
    ANSITerminal.erase ANSITerminal.Screen;
    ANSITerminal.set_cursor 1 1;
    ANSITerminal.(print_string [yellow] 
                    ("Please enter \"load\" to load a previous Monopoly game\n"
                     ^ "or \"new\" to start a new Monopoly game.\n";));
    print_string "> ";
    match String.lowercase_ascii (read_line ()) with 
    | "load" -> load_game true
    | "new" -> play_new_game true
    | _ -> load_or_save () in
  load_or_save ()

(* Execute the game engine. *)
let () = main ()