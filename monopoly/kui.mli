(** The type implementing the visual changes to the monopoly board. *)

(** [get1char n st c] is the character value of the next key either pressed by 
    the user or submitted by the bot depending on whether it's a human's or
    bot's turn in state [st]. The choice prompting this user input is [c].
    Player name [n] is only needed to determine the current player when it is
    not known in [st], i.e. when the game is being initialized.

    code for this function taken from https://stackoverflow.com/questions
    /13410159/how-to-read-a-character-in-ocaml-without-a-return-key *)
val get1char : ?player_name:string -> State.t -> Bot.bot_choice -> char

(** [determine_start_player players first_player index state game] 
    prints the initial state [state] of the board and animates
    the initial dice rolls to decide which player will go first, highlighting
    the "go" tile the color of [first_player] with player index [index] in the
    [game]. [players] is a list of the players in the game mapped to their 
    initial dice rolls. *)
val determine_start_player : (string * (int * int)) list -> string -> int -> 
  State.t -> Game.t -> unit

(** [disp_turn current_player player_number current_tile] 
    prints relevant information for the [current_player] with player
    index [player_number] at the beginning of their turn. The [current_player] 
    is on the tile with name [current_tile]. *)
val disp_turn : string -> int -> string -> unit

(** [roll_dice current_player index dice old_loc game] animates a [dice] roll
    for the [current_player] with player number [index], moving them from the 
    tile with id [old_loc] to the next tile specified by the [dice] roll in the
    [game].*)
val roll_dice : string -> int -> int * int -> int -> Game.t -> unit

(** [buy_property property_id player_index game] changes the inner color of the
    box of the tile with id [property_id] to the player's color (who has an 
    index of [player_index]) in the [game] *)
val buy_property : int -> int -> Game.t -> unit

(** [disp_buy_message property_id game] prints a message to the user, asking
    them if they would like to purchase the property with id [property_id]
    in the [game]. *)
val disp_buy_message : Game.tile_id -> Game.t -> unit

(** [confirm_buy st] displays a confirmation message for the purchase of a 
    property in state [st]. *)
val confirm_buy : State.t -> unit

(** [deny_buy st] displays a denial message for the purchase of a property 
    in state [st]. *)
val deny_buy : State.t -> unit

(** [disp_change_turn old_tile new_player new_tile game] animates the changing
    of turns to the player with index [new_player], unhighlighting the 
    [old_tile] and highlighting the [new_tile] with the color of the new player 
    in the [game]. *)
val disp_change_turn : int -> int -> int -> Game.t -> unit

(** [disp_player_info player_list] prints the information in [player_list] to 
    the screen, where [player_list] is the list of players mapped to their
    current amounts of cash. *)
val disp_player_info : (string * int) list -> unit

(** [disp_rent_message is_utility roll prop_name owner_name rent st] informs the 
    user that they owe [owner_name] [rent] amount of money, upon landing on 
    [prop_name] in state [st]. If the property landed on is a utility, 
    then [is_utility] is true, and [roll] is the pair of ints rolled to 
    determine the amount owed for that utility's rent *)
val disp_rent_message : ?is_utility:bool -> ?roll:int*int -> string -> string 
  -> int -> State.t -> unit

(** [disp_menu highlight_line] prints the main menu with menu line 
    [highlight_line] highlighted red. *)
val disp_menu : int -> unit

(** [clear_menu] clears an active menu from the screen *)
val clear_menu : unit -> unit

(** [print_player_properties prop_names game] prints the list of property tile 
    ids with their mortgage values, represented by [prop_names], to the screen 
    in the [game]. If the property is mortgaged, it will be highlighted red. 
    Each individual entry in [prop_names] is a (tile_id, mortgage_amt, 
    is_mortgaged, num_houses) *)
val print_player_properties : (int * int * bool * int) list -> Game.t -> unit

(** [print_mortgage_prompts mapping game] prints individual characters next to 
    properties which represents what keypress will mortgage a property in the 
    [game].
    [mapping] is a list of property_ids mapped to the character displayed next 
    to them*)
val print_property_prompts : (int * string) list -> Game.t -> unit

(** [disp_chance_message] informs the user that they have landed on a chance 
    tile *)
val disp_chance_message : unit -> unit

(** [disp_tax_message amt st] informs the user that they owe [amt] dollars as a 
    tax in state [st]. *)
val disp_tax_message : int -> State.t -> unit

(** [disp_go_to_jail ()] informs the user that they have been sent to jail. *)
val disp_go_to_jail : unit -> unit

(** [disp_jail_doubles ()] informs the user that they have been sent to 
    jail because they rolled three consecutive doubles. *)
val disp_jail_doubles : unit -> unit

(** [disp_jail_fine ()] informs the user that they must pay $50 to leave
    jail. *)
val disp_jail_fine : unit -> unit 

(** [disp_stuck_in_jail ()] informs the user that they are stuck in jail
    becuase they haven't rolled a double. *)
val disp_stuck_in_jail : unit -> unit

(** [animate_jail player_index current_tile game st] animates sending the player
    with index [player_index] whose current position is [current_tile] to jail
    in the state [st] of [game] *)
val animate_jail : int -> int -> Game.t -> State.t -> unit

(** [disp_rolled_double st] prints the message that the player rolled a double
    in state [st] and is therefore allowed to roll again, 
    continuing their turn. *)
val disp_rolled_double : State.t -> unit 

(** [pause_message st] prints the message that the user should see when landing 
    on non-property tiles, pausing to allow them to see where they are on the 
    board in state [st] before moving on to the next player's turn. *)
val pause_message : State.t -> unit

(** [raise_cash_menu highlight_line] prints the menu the user sees when needing
    to raise more cash with line [highlight_lines] highlighted *)
val raise_cash_menu : int -> unit

(** [print_out_of_cash_message amt_owed amt_needed] prints a message to the 
    console, informing them that they owe [amt_owed] and need [amt_needed]
    to finish the transaction *)
val print_out_of_cash_message : int -> int -> unit

(** [clear_cash_menu] clears the cash menu from the screen *)
val clear_cash_menu : unit -> unit

(** [mortgage_message] diplays a message to the screen informing the player
    how to mortgage their mortgageable properties *)
val mortgage_message : unit -> unit

(** [disp_buy_house id num_houses game] displays [num_houses] on the property
    with [id] in the [game] *)
val disp_buy_house : Game.tile_id -> int -> Game.t -> unit

(** [print_house_message] informs the user that they have the option to buy
    houses on properties by selecting the button corresponding to that 
    property *)
val print_house_message : unit -> unit

(** [disp_monopoly_message] informs the user that they must have a monopoly
    to buy houses on property *)
val disp_monopoly_message : unit -> unit

(** [disp_too_many_houses] informs the user that they can't buy any more
    houses since they have 5 already *)
val disp_too_many_houses : unit -> unit

(** [disp_invalid_mortgage] informs the user that they can't mortgage the 
    chosen property *)
val disp_invalid_mortgage : unit -> unit

(** [disp_no_houses] informs the user that they have no houses to sell *)
val disp_no_houses : unit -> unit

(** [animate_dice time_duration dice1 dice2] prints a dice animation to the 
    screen, with the final number of dots on the first dice being [dice1], 
    and the final number of dots on the second dice being [dice2]. This 
    animation lasts for [time_duration] seconds. *)
val animate_dice : ?time_duration:float -> int -> int -> unit

(** [print_board_from_file owners current_player current_tile players_cash
    tile_houses game] prints the board to the screen based on information
    loaded from a file, which includes a list of [owners] mapped to their
    tile_ids, the index of the [current_player], their [current_tile],
    a list of the [players_cash] which is their name mapped to the amount
    of cash they have, a list of [tile_houses] which is all of the 
    tile_ids on the board mapped to the number of houses on them, and the 
    current [game]. *)
val print_board_from_file : (int * Game.tile_id) list -> int -> Game.tile_id -> 
  (string * int) list -> (Game.tile_id * int) list -> Game.t -> unit

(** [disp_no_railroad_house] informs the user that they can't buy any houses
    on a current tile if it is a railroad *)
val disp_no_railroad_house : unit -> unit

(** [redistribute_property player_property state game] animates a player
    losing the game, unhighlighting all of their properties from the [game]
    with state [state]. [player_property] maps the player index to a list
    of all their owned properties. *)
val redistribute_property : int * Game.tile_id list -> State.t -> Game.t -> unit

(** [win_message st] informs the player that they have won in the game with
    state [st] *)
val win_message : State.t -> unit

(** [card_message msg st] prints the [msg], representing a draw card, to 
    the screen in a game with current state [st] *)
val card_message : string -> State.t -> unit

(** [animate_card_move start_id end_id player_id game] represents moving
    the player with index [player_id] in the [game] from tile [start_id] to 
    [end_id] when drawing a card *)
val animate_card_move : int -> int -> int -> Game.t -> unit

(** [force_payment amt st] prints out the [amt] of money you were 
    forced to transfer in a game with current state [st]. *)
val forced_payment : int -> State.t -> unit