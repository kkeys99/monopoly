open OUnit2
open State
open Game
open Command
open Save

let make_test msg correct execution =
  msg >:: (fun _ -> assert_equal (correct) (execution))
let make_test_err msg exnp execution =
  msg >:: (fun _ -> assert_raises (exnp) (fun () -> (execution())) )

(** [make_test_file file] loads a file with name [file] 
  into a Yojson.Basic.json *)
let rec make_test_file (file : string) : Yojson.Basic.json = 
  try Yojson.Basic.from_file file with 
  | Sys_error e -> 
    ANSITerminal.(print_string [red] "Invalid JSON file name. Try again.\n");
    print_string "> ";
    match read_line () with 
    | file_name -> make_test_file file_name

(** [extract_from_property g id getter] extracts the 
value from [getter] of the property at id [id] from game [g]  *)
let extract_from_property (g) (id) (getter) = 
  (get_property_by_id id g) |> getter 

(* setup for simple game testing *)
let g = make_test_file "simple-monopoly.json" |> Game.from_json "monopoly.json"

let game_tests = [  
  make_test "get all tiles ID's" 
  [38; 36; 33; 30; 22; 20; 17; 10; 7; 4; 2; 0; 5; 3; 1] 
  (get_tile_ids g);
  make_test "get all property ID's" [5;3;1] 
    (get_property_ids g);
  make_test "get correct property" 3 
    (extract_from_property g 3 id_of_property);
  make_test "property ID's to colors" [
    (1, ANSITerminal.Background ANSITerminal.White); 
    (3, ANSITerminal.Background ANSITerminal.White); 
    (5, ANSITerminal.Background ANSITerminal.Default)
  ] (map_id_to_color g);
  make_test "property ID's to names" [
    (0, "Go <------");
    (2, "Community Chest");
    (4, "Income Tax");
    (7, "Chance ?");
    (10, "Just Visiting");
    (17, "Community Chest");
    (20, "Free Parking");
    (22, "Chance ?");
    (30, "To Jail");
    (33, "Community Chest");
    (36, "Chance ?");
    (38, "Luxury Tax");
    (1, "Mediterranean Avenue");
    (3, "Baltic Avenue");
    (5, "Reading Railroad")
  ] (map_id_to_name g);
  make_test "mortgage value: normal" 30 
    (extract_from_property g 1 mortgage_value);
  make_test "mortgage value: railroad" 100 
    (extract_from_property g 5 mortgage_value);
  make_test "building cost: normal" 50 
    (extract_from_property g 3 building_cost);
  make_test "building cost: railroad" 0 
    (extract_from_property g 5 building_cost);
  make_test "id of property: normal" 3 
    (extract_from_property g 3 id_of_property);
  make_test "id of property: railroad" 5 
    (extract_from_property g 5 id_of_property);
  make_test "sale_price: normal" 60 
    (extract_from_property g 3 sale_price);
  make_test "sale_price: railroad" 200 
    (extract_from_property g 5 sale_price);
  make_test "rent: normal" 4 
    (extract_from_property g 3 rent);
  make_test "rent: railroad" 25 
    (extract_from_property g 5 rent);
  make_test "rent1: normal" 20 
    (extract_from_property g 3 rent1);
  make_test "rent1: railroad" 50 
    (extract_from_property g 5 rent1);
  make_test "rent2: normal" 60 
    (extract_from_property g 3 rent2);
  make_test "rent2: railroad" 100 
    (extract_from_property g 5 rent2);
  make_test "rent3: normal" 180 
    (extract_from_property g 3 rent3);
  make_test "rent3: railroad" 200 
    (extract_from_property g 5 rent3);
  make_test "rent4: normal" 320 
    (extract_from_property g 3 rent4);
  make_test "rent4: railroad" 0 
    (extract_from_property g 5 rent4);
  make_test "hotel_rent: normal" 450 
    (extract_from_property g 3 hotel_rent);
  make_test "hotel_rent: railroad" 0 
    (extract_from_property g 5 hotel_rent);
  make_test "get_property_by_id: success" 3 
    (get_property_by_id 3 g |> id_of_property);
  make_test_err "get_property_by_id: Go tile failure" (Not_found) 
    (fun _ -> get_property_by_id 0 g);
  make_test_err "get_property_by_id: edge case" (Not_found) 
    (fun _ -> get_property_by_id 40 g);
  make_test_err "get_property_by_id: impossible" (Not_found) 
    (fun _ -> get_property_by_id 999 g);
  make_test "get 2 colors" [1;3] 
    (get_property_by_id 3 g |> get_property_set g);
  make_test "get 1 colors" [5] 
    (get_property_by_id 5 g |> get_property_set g);
]

let full_g = make_test_file "monopoly.json" |> Game.from_json "monopoly.json"

(* move_forward states *)
let state = init_state (full_g)(["Greg"; "Charlie"; "Kevin"; "Bobby"]) ("Greg")
let move_forward_by_0 = move_forward state 0
let move_forward_by_1 = move_forward state 1
let move_forward_income_tax = move_forward state 4
let move_forward_utility = move_forward state 12

(* buy_property states *)
let buy_reading_railroad = buy_property (move_forward state 5) 5 full_g
let buy_baltic = buy_property (move_forward state 3) 3 full_g
let buy_electric = buy_property (move_forward state 12) 12 full_g

(* rent *)
let charlie_land_on_reading_railroad = 
  move_forward (buy_reading_railroad |> change_turn) 5 

(* check bought properties *)
let cash_check_reading_railroad =  
  get_player "Greg" buy_reading_railroad |> get_player_cash
let cash_check_baltic =  
  get_player "Greg" buy_baltic |> get_player_cash
let cash_check_electric =  
  get_player "Greg" buy_electric |> get_player_cash

(* mortage properties *)
let mortgage_baltic = mortgage_property (buy_baltic) 3 full_g
let is_mortgaged_baltic = is_mortgaged (mortgage_baltic) 3
let unmorgage_baltic = unmortgage_property (mortgage_baltic) 3 full_g
let is_unmortgaged_baltic = is_mortgaged (unmorgage_baltic) 3

(** [load s] loads state [s] into the gamestate  *)
let load s = make_test_file s |> State.from_json

(* deep game state tests *)
let game_state_full = load "tests/game_state_test.json"
let on_luxury_tax = load "tests/on_luxury_tax.json"
let on_card_tile = load "tests/on_card_tile.json"
let on_utility = load "tests/on_utility.json"
let should_go_to_jail = load "tests/should_go_to_jail.json"
let in_jail_1_turn = load "tests/in_jail_1_turn.json"
let victory = load "tests/victory.json"

(* curated map from ingame anaylsis  *)
let id_to_houses = [(1, 0); (3, 0); (5, 0); (6, 5); (8, 1); (9, 5); (11, 0); 
(12, 0); (13, 0); (14, 0); (15, 0); (16, 0); (18, 0); (19, 0); (21, 0); 
(23, 0); (24, 0); (25, 0); (26, 0); (27, 0); (28, 0); (29, 0); (31, 0); 
(32, 0); (34, 0); (35, 0); (37, 1); (39, 0)]

let state_tests = [
  make_test "is_buyable: true" (true) 
    (property_is_buyable state 3);
  make_test "is_buyable: false" (false) 
    (property_is_buyable state 999);
  make_test "get_player_index, first" 0 
    (get_player_index state "Greg");
  make_test "get_player_index, third" 2 
    (get_player_index state "Kevin");
  make_test_err "get_player_index, precondition failure" 
    (Failure "precondition violated in first_player_index") 
    (fun _ -> get_player_index state "Nate the Great Foster");
  make_test "change_turn 0->1" 1 
    (state |> change_turn |> get_current_player_index);
  make_test "change_turn 0->1->2->3->0" 0 
    (state |> 
      change_turn |> 
      change_turn |> 
      change_turn |> 
      change_turn |> get_current_player_index);
  make_test "current player name default" "Greg" 
    (state |> current_player_name);
  make_test "current player one turn" "Charlie" 
    (state |> change_turn |>  current_player_name);
  make_test "move to same tile" 0 
    (move_to state 0 |> current_player_tile_id);
  make_test "move to different tile" 10 
    (move_to state 10 |> current_player_tile_id);
  make_test "move forward by 0" 0 
    (move_forward state 0 |> current_player_tile_id);
  make_test "move forward by 3" 3 
    (move_forward state 3 |> current_player_tile_id);
  make_test "move forward by max tiles" 1 
    (move_forward state 41 |> current_player_tile_id);
  make_test "current player tile id start" 0 
    (move_forward_by_0 |> current_player_tile_id);
  make_test "current player tile id move 1" 1 
    (move_forward_by_1 |> current_player_tile_id);
  make_test "current player tile move 0" "Go <------" 
    (current_player_tile move_forward_by_0 full_g);
  make_test "current player tile move 1" "Mediterranean Avenue" 
    (current_player_tile move_forward_by_1 full_g);  
  make_test "player buys railroad" (1500-200) 
    (cash_check_reading_railroad);
  make_test "player buys property" (1500-60) 
    (cash_check_baltic);
  make_test "player buys utility" (1500-150) 
    (cash_check_electric);
  make_test "current position is income tax tile: valid"  (true) 
    (current_pos_is_tax (move_forward_income_tax) full_g);
  make_test "current position is income tax tile: invalid"  (false) 
    (current_pos_is_tax (move_forward_by_0) full_g);   
  make_test "current position is utiltiy tile: valid"  (true) 
    (current_pos_is_utility (move_forward_utility) full_g);
  make_test "current position is utiltiy tile: invalid"  (false) 
    (current_pos_is_utility (move_forward_by_0) full_g);   
  make_test "mortgage successful"  (true) 
    (is_mortgaged_baltic);   
  make_test "unmortgage successful"  (false) 
    (is_unmortgaged_baltic);
  make_test "property_is_buyable: normal true" (true) 
    (property_is_buyable state 3);
  make_test "property_is_buyable: normal false" (false) 
    (property_is_buyable buy_baltic 3);
  make_test "property_is_buyable: corner tile" (false) 
    (property_is_buyable state 0);
  make_test "property_is_buyable: chance" (false) 
    (property_is_buyable state 7);
  make_test "get owner of property: success" (Some "Greg") 
    (get_owner_of_property buy_baltic 3);
  make_test "get owner of property: no owner" (None) 
    (get_owner_of_property state 3);
  make_test "tile ids by player index, 0 owned" ([]) 
    (tile_ids_by_player_index state 0);
  make_test "tile ids by player index, 1 owned" ([5]) 
    (tile_ids_by_player_index buy_reading_railroad 0);
  make_test "cash list starting" (
    [("Greg", 1500); ("Charlie", 1500); ("Kevin", 1500); ("Bobby", 1500)]) 
    (player_cash_list state);
  make_test "cash list updated +" (
    [("Greg", 1600); ("Charlie", 1500); ("Kevin", 1500); ("Bobby", 1500)]) 
    (player_cash_list (update_cash state 100 "Greg"));
  make_test "cash list updated -" (
    [("Greg", 1400); ("Charlie", 1500); ("Kevin", 1500); ("Bobby", 1500)]) 
    (player_cash_list (update_cash state (-100) "Greg"));
  make_test "cash list 1 buy" (
    [("Greg", 1500-200); ("Charlie", 1500); ("Kevin", 1500); ("Bobby", 1500)]) 
    (player_cash_list buy_reading_railroad);
  make_test "get rent: normal" (2) 
    (get_rent state 1 full_g 0);
  make_test "get rent: railroad" (25) 
    (get_rent buy_reading_railroad 5 full_g 0);
  make_test "get rent: utility" (4) 
    (get_rent buy_electric 12 full_g 1);
  make_test "Charlie should pay rent on reading railroad (not owned)" (true) 
    (should_pay_rent charlie_land_on_reading_railroad 5);
  make_test "Greg don't pay rent on self-owned tile" (false) 
    (should_pay_rent buy_reading_railroad 5);
  make_test "Don't pay rent on non-purchased tile" (false) 
    (should_pay_rent state 5);
  make_test "get_num_houses_by_id: 0" (0) 
    (get_num_houses_by_id state 5);
  make_test "get_num_houses_by_id: 0 (selloff)" (0) 
    (get_num_houses_by_id (sell_house(buy_house state 5 full_g) 5 full_g) 5);
  make_test "should_current_go_to_jail: Init" (false) 
    (should_current_go_to_jail state);
  make_test "0 mortgagable properties" ([]) 
    (current_player_mortgagable_props state);
  make_test "1 mortgagable properties" ([3]) 
    (current_player_mortgagable_props buy_baltic);
  make_test "UnMortgage owned Property" (false) 
    (is_mortgaged (toggle_mortgage_pub game_state_full 16 full_g) 16);
  make_test "Mortgage owned Property" (true) 
    (is_mortgaged (toggle_mortgage_pub game_state_full 28 full_g) 28);
  make_test "Player send to Jail" (true) 
    ( send_to_jail game_state_full "greg" |> get_player "greg" |> is_in_jail);
  make_test "Mortgageable properties: none" ([]) 
    (current_player_mortgagable_props state);
  make_test "Mortgageable properties: mulitple"([3;16; 25; 26; 29; 32; 34; 39]) 
    (current_player_mortgagable_props game_state_full);
  make_test "Current position is utility: False" (false) 
    (current_pos_is_utility game_state_full full_g);
  make_test "Current position is utility: True" (true) 
    (current_pos_is_utility on_utility full_g);
  make_test "Current position is tax: False" (false) 
    (current_pos_is_tax game_state_full full_g);
  make_test "Current position is tax: True" (true) 
    (current_pos_is_tax on_luxury_tax full_g);
  make_test "Current position is card: False" (false) 
    (current_pos_is_card game_state_full full_g);
  make_test "Current position is card: True" (true) 
    (current_pos_is_card on_card_tile full_g);
  make_test "Current player should go to jail consec rolls: True" (true) 
    (should_current_go_to_jail should_go_to_jail);
  make_test "Current player should go to jail tile 30: True" (true) 
    (should_current_go_to_jail (move_to state 30));
  make_test "Current player should go to jail: False" (false) 
    (should_current_go_to_jail state); 
  make_test "Current player should go to jail: False" (false) 
    (should_current_go_to_jail state); 
  make_test "Current player in Game " (true) 
    (current_player_in_game (state)); 
  make_test "property cost regular property: baltics" (60) 
    (property_cost state 3 full_g);
  make_test "property cost railroad property: reading rr" (200) 
    (property_cost state 5 full_g);
  make_test "property cost utility property: waterworks" (150) 
    (property_cost state 28 full_g);
  make_test "tiles_owned_by_player: nate" ([3;16;25;26;29;32;34;37;39]) 
    (tile_ids_owned_by_player game_state_full "nate");
  make_test "Reset consecutive doubles" 
    (state) (reset_consec_doubles state);
  make_test "Props of Owners" ([]) 
    (props_of_owners state);
  make_test "Props of Owners: 1 owned" 
    ([(0, 3)]) (props_of_owners buy_baltic);
  make_test "Game is NOT over" (false) 
    (game_is_over state);
  make_test "Game is REALLY over" (true) 
    (game_is_over victory);
  make_test "Properties with houses: None" ([]) 
    (properties_with_houses state full_g);
  make_test "Properties with houses owned by single user: 1" ([37]) 
    (properties_with_houses game_state_full full_g);
  make_test "Properties with houses owned by single user: Multi" ([6;8;9]) 
    (properties_with_houses (game_state_full|>change_turn) full_g);
  make_test "Buildable properties, no monopolies" ([]) 
    (buildable_properties state full_g);
  make_test "Buildable properties (monopoly on dark blue )" ([37; 39]) 
    (buildable_properties game_state_full full_g);
  make_test "mortgageable_properties: 1 already mortgaged"
    ([3; 25; 26; 29; 32; 34; 39]) (mortgageable_properties game_state_full);
  make_test "unmortgageable_properties: 1 already mortgaged"
    ([16]) (unmortgageable_properties game_state_full);
  make_test "is mortgageable by owner" (true) 
    (prop_is_mortgageable game_state_full 39);
  make_test "is NOT mortgageable by owner (already mortgaged)" (true) 
    (prop_is_mortgageable game_state_full 16);
  make_test "minimum cost to build: default" (None)   
    (min_cost_to_build state full_g);
  make_test "minimum cost to build variable" (Some 200) 
    (min_cost_to_build game_state_full full_g);
  make_test "minimum expansion cost: default" (None)   
    (min_expand_cost state full_g);
  make_test "minimum expansion cost variable" (Some 99)   
    (min_expand_cost game_state_full full_g);
  make_test "minimum mortgage cost: default" (None)   
    (min_cost_to_unmortgage state full_g);
  make_test "minimum mortgage cost variable" (Some 99)   
    (min_cost_to_unmortgage game_state_full full_g);
  make_test "Valid monopoly" (true) 
    (is_monopoly game_state_full 39 full_g); 
  make_test "Invalid monopoly" (false) 
    (is_monopoly state 39 full_g);
  make_test "0 turns in jail" (0)
    (current_turns_in_jail game_state_full);
  make_test "2 turn in jail" (2)
    (in_jail_1_turn |> current_turns_in_jail);
  make_test "Current consecutive doubles: 0" (0)
    (game_state_full |> current_consec_rolls);
  make_test "Current consecutive doubles: 1" (1)
    (in_jail_1_turn |> current_consec_rolls);
  make_test "Current consecutive doubles: 2" (2)
    ( incr_consec_roll (incr_consec_roll in_jail_1_turn "greg") "greg" 
    |> current_turns_in_jail );
  make_test "ids to houses midgame" (id_to_houses)
    (ids_to_num_houses game_state_full);
  make_test "current player enough cash success" (true) 
    (current_player_enough_cash game_state_full 100);
  make_test "current player enough cash failure" (false) 
    (current_player_enough_cash game_state_full 500);
  make_test "Current player assets, starting cash only" (1500) 
    (current_player_assets state full_g);
  make_test "Current player assets, midgame" (1630) 
    (current_player_assets game_state_full full_g);
  make_test "Current player passes Go: fail" (game_state_full) 
    (new_state_passed_go game_state_full 0);
  make_test "Current player passes Go: success" 
    (current_player_assets game_state_full full_g + 200) 
    (current_player_assets (new_state_passed_go game_state_full 17) full_g);
  make_test "Mortgage cost: unmortgaged property" (150) 
    (get_mortgage_cost game_state_full 32 full_g);
  make_test "Unmortgage cost: mortgaged property property" (99) 
    (get_mortgage_cost game_state_full 16 full_g);
]
let command_tests = [ 
  make_test "Roll ' '" (Roll)
    (parse ' ');
  make_test "Menu '\027'" (Menu)
    (parse '\027');
  make_test "Parse Malformed" (Malformed)
    (parse 'N');
  make_test "Yes 'y'" (Yes)
    (parse_confirm 'y');
  make_test "No 'n'" (No)
    (parse_confirm 'n');
  make_test "Parse_confirm Malformed" (ChoiceMalformed)
    (parse_confirm 'N');
  make_test "Menu option 0, Mortgage" (Mortgage)
    (parse_menu_option 0);
  make_test "Menu option 1, Buy" (Buy)
    (parse_menu_option 1);
  make_test "Menu option 2, Sell" (Sell)
    (parse_menu_option 2);
  make_test "Menu option 3, Resign" (Resign)
    (parse_menu_option 3);
  make_test "Menu option 4, Save" (Save)
    (parse_menu_option 4);
  make_test "Menu option 5, Exit" (Exit)
    (parse_menu_option 5);
  make_test "Menu option 6, Close" (Close)
    (parse_menu_option 6);
  make_test "Menu option _, Malformed" (Malformed)
    (parse_menu_option 999);
  make_test "Cash Menu option 0: Mortgage" (Mortgage)
    (parse_cash_menu_option 0);
  make_test "Cash Menu option 1: Sell" (Sell)
    (parse_cash_menu_option 1); 
  make_test "Cash Menu option 2: Resign" (Resign)
    (parse_cash_menu_option 2); 
  make_test "Cash Menu option 3: Close" (Close)
    (parse_cash_menu_option 3); 
  make_test "Cash Menu option _: Malformed" (Malformed)
    (parse_cash_menu_option 999); 
]
let suite = "game test suite" >::: List.flatten[
    game_tests;
    state_tests;
    command_tests;
  ]
let _ = run_test_tt_main suite 