open Game
open Bot

(** The type representing box printing of properties on the board *)
type p_type = Horiz | Vert | TLeft | TRight | BLeft | BRight

(** The six possible colors of a player, based on the order their names are
    entered by the user *)
let player_colors = [ANSITerminal.on_blue; ANSITerminal.on_green; 
                     ANSITerminal.on_cyan; ANSITerminal.on_red; 
                     ANSITerminal.on_magenta; ANSITerminal.on_yellow]
(** The location where a player's name and cash value is printed to the 
    screen *)
let player_info_locs = [0, (53,46); 1, (104,46); 
                        2, (53,47); 3, (104,47);
                        4, (53,48); 5, (104,48)]
(** A safe location where users can type, so graphics won't be overwritten *)
let typespace = (20,160)
(** The location where game messages are printed to the screen*)
let console = (20, 6)
(** The location where property information is printed to the screen *)
let property_console = (21, 12)
(** The height of the console, in rows of text *)
let console_height = 10
(** The width of the console, in columns of text *)
let console_width = 152
(** The width of the main menu *)
let menu_width = 34
(** The width of the cash meny *)
let cash_menu_width = 25
(** The x-coordinate of the main menu *)
let menu_x = 59
(** The location for printing dice to the screen *)
let dice = (85, 35)
(** The amount of space between the two dice *)
let dice_spacing = 12
(** The time delay between printing dice when rolling *)
let dice_delay = 0.07
(** The width of a property tile *)
let property_width = 18
(** A mapping of tile ids to their locations on the screen for printing *)
let tile_locations = [0, (171, 41); 1, (154, 41); 2, (137, 41); 3, (120, 41); 
                      4, (103, 41); 5, (86, 41); 6, (69, 41); 7, (52, 41); 
                      8, (35, 41); 9, (18,41); 10, (1,41); 11, (1, 37); 
                      12, (1,33); 13, (1,29); 14, (1,25); 15, (1,21); 
                      16, (1,17); 17, (1,13); 18, (1,9); 19, (1,5); 
                      20, (1,1); 21, (18,1); 22, (35,1); 23, (52,1); 
                      24, (69,1); 25, (86,1); 26, (103,1); 27, (120,1); 
                      28, (137,1); 29, (154,1); 30, (171,1); 31, (171, 5); 
                      32, (171,9); 33, (171, 13); 34, (171, 17); 35, (171, 21);  
                      36, (171, 25); 37, (171, 29); 38, (171, 33); 
                      39, (171, 37)]

let get1char 
    ?(player_name = "") 
    (state : State.t)
    (choice : bot_choice) : char =
  let is_bot = (player_name = "") && 
               String.contains (State.current_player_name state) bot_prefix in
  if (String.contains player_name bot_prefix) || is_bot then 
    bot_parse choice state
  else 
    begin
      let termio = Unix.tcgetattr Unix.stdin in
      let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
      let res = input_char stdin in
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
      res
    end

(** [get_print_player_info_loc player_num] is the location of the game 
    information, namely name and cash value, which needs to be printed to the 
    screen *)
let get_print_player_info_loc (player_num : int) : (int * int) = 
  List.assoc player_num player_info_locs

(** [get_player_color id] is the color of player with player index [id]. 
    Raises [Failure] if [id] is not a valid player index *)
let get_player_color (id : int) : ANSITerminal.style = 
  try List.nth player_colors id with _ -> failwith "error in get_player_color"

(** [get_tile_loc id] is the location of the tile with tile_id [id] on the 
    screen*)
let get_tile_loc (id : int) : (int * int) = List.assoc id tile_locations

(** [get_tile_name id game] is the name of the tile with tile_id [id] in the
    [game]. If the [id] is invalid, returns dummy value "Hello World" *)
let get_tile_name (id : int) (game : Game.t) : (string * string) = 
  try
    let name_list = Game.map_id_to_name game in
    let str_list = String.split_on_char ' ' (List.assoc id name_list) in
    match str_list with
    | s1::s2::[] -> (s1,s2)
    | _ -> failwith "error in get_tile_name"
  with _ -> ("Hello", "World")

(** [get_tile_color id game] is the color of the property with tile_id [id]
    in the [game] *)
let get_tile_color (id : int) (game : Game.t) : ANSITerminal.style = 
  try 
    let color_list = Game.map_id_to_color game in 
    (List.assoc id color_list)
  with _ -> ANSITerminal.on_default

(** [get_tile_foreground_color id game] returns the color 
    of the tile with tile_id [id] in the [game] as a foreground color *)
let get_tile_foreground_color (id : int) (game : Game.t) =
  match get_tile_color id game with
  | ANSITerminal.Background ANSITerminal.Black -> ANSITerminal.white
  | ANSITerminal.Background ANSITerminal.Blue -> ANSITerminal.blue
  | ANSITerminal.Background ANSITerminal.Cyan -> ANSITerminal.cyan
  | ANSITerminal.Background ANSITerminal.Green -> ANSITerminal.green
  | ANSITerminal.Background ANSITerminal.Magenta -> ANSITerminal.magenta
  | ANSITerminal.Background ANSITerminal.Red -> ANSITerminal.red
  | ANSITerminal.Background ANSITerminal.White -> ANSITerminal.white
  | ANSITerminal.Background ANSITerminal.Yellow -> ANSITerminal.yellow
  | _ -> ANSITerminal.on_default

(** [get_tile_type id] is the [p_type] of the property with tile_id [id] *)
let get_tile_type (id : int) : (p_type) =
  if id = 0 then BRight else 
  if id = 10 then BLeft else 
  if id = 20 then TLeft else 
  if id = 30 then TRight else
  if (id < 30 && id > 20) || (id > 0 && id < 10) then Horiz else Vert

(** [print_in_place text loc] prints [text] to the screen at location [loc] 
    without a newline character at the end. If optional arguments [f_color]
    or [b_color] are supplied, the text will have foreground color [f_color]
    and background color [b_color]. *)
let print_in_place ?(f_color = ANSITerminal.white) 
    ?(b_color = ANSITerminal.on_default) 
    (text : string list) (loc : int * int) : unit =
  let mult_lines = List.length text <> 1 in
  let rec helper text =
    match text with
    | [] -> ()
    | word::t ->
      let length = if mult_lines then String.length word else 0 in
      ANSITerminal.print_string [f_color; b_color] word;
      ANSITerminal.move_cursor ~-length 1;
      helper t
  in ANSITerminal.set_cursor (fst loc) (snd loc);
  helper text

(** [typeable] moves the cursor on the screen to location [typespace] *)
let typeable () : unit = ANSITerminal.set_cursor (fst typespace) (snd typespace)

(** [center width word] is a string of [length] where [word] 
    is in the exact center of the string with
    the appropriate delimiters. Note for some cases this isn't exactly possible, 
    like where there is an odd fixed length and even length word *)
let center (length : int) (word : string) : string = 
  let rec center_help length acc bool =
    if length <= 0 then acc 
    else
    if bool then center_help (length-1) (" " ^ acc) false
    else center_help (length-1) (acc ^ " " ) true
    (* subtract the length of the word and the total length of the visible 
       delimiters *)
  in (center_help (length - 2 - String.length word) word true)

(** [print_to_console text loc] prints [text] in place, where [loc] is the 
    [text]'s location relative to the console, (0,0) being the upper left corner
    of the console. If optional arguments [b_color] or [f_color] are supplied,
    the text will have background color [b_color] and foreground color 
    [f_color] *)
let print_to_console ?(b_color = ANSITerminal.on_default) 
    ?(f_color = ANSITerminal.white) (text : string) (loc : int * int) : unit =
  print_in_place ~f_color:f_color ~b_color:b_color [text] 
    (fst loc + fst console, snd loc + snd console)

(** [print_to_property_console] prints the property with name [text], foreward
    [color] and background color [b_color] to the property console with 
    location [loc] *)
let print_to_property_console ?(b_color = ANSITerminal.on_default) 
    (text : string) (loc : int * int) (color : ANSITerminal.style) = 
  print_to_console ~b_color:b_color ~f_color:color text 
    (fst loc + 1, snd loc + 12)

(** [print_banner text line color] centers the [text] in the console, and prints
    it to the console [line], where [line] 0 is the first line in the console,
     with background [color]. If optional argument [text_color] is supplied, 
     the [text] will have foreground color [text_color] *)
let print_banner ?(text_color = ANSITerminal.white) (text : string) (line : int)
    (color : ANSITerminal.style) : unit =
  print_to_console ~f_color:text_color ~b_color:color 
    (center console_width text) (0, line)

(** [menu_banner text line] prints a menu banner to the screen at the console 
    [line].
    A menu banner which is like a normal banner but with width [menu_width]. 
    The menu banner displays [text], and is highlighted red if optional argument
    [highlight] is true. Otherwise, the banner will be cyan. *)
let menu_banner ?(highlight=false) text line width =
  if highlight then 
    (print_to_console ~b_color:ANSITerminal.on_cyan ~f_color:ANSITerminal.black 
       ("║") (menu_x, line);
     print_to_console ~b_color:ANSITerminal.on_red ~f_color:ANSITerminal.black 
       (center (width - 4) (String.sub text 3 (width-4))) 
       (menu_x + 1, line);
     print_to_console ~b_color:ANSITerminal.on_cyan ~f_color:ANSITerminal.black 
       ("║") (menu_x + width-3, line))
  else
    print_to_console ~b_color:ANSITerminal.on_cyan ~f_color:ANSITerminal.black 
      (center width text) (menu_x, line)

(** [clear_line line] clears the console [line] *)
let clear_line line = 
  print_in_place ["                                                                                                                                                       "]
    (fst console, snd console + line)

(** [clear_console] clears all the lines of the console *)
let clear_console () = 
  let rec helper line =
    if line <= 10 then (clear_line line; helper (line + 1)) else ()
  in helper 0

(** [produce_property_text word loc] prints the centered [word] in place
    at location [loc], bookended by the box-building character "│", which
    has color [side_color], if it is supplied *)
let produce_property_text ?(side_color = ANSITerminal.on_white) (word: string) (
    loc : int * int) : unit = 
  print_in_place ~b_color:side_color ["│"] loc;
  print_in_place [center property_width word] (fst loc + 1, snd loc);
  print_in_place ~b_color:side_color ["│"] (fst loc + 17, snd loc)

(** [print_property_color color loc] prints out the [color] of a property
    located at [loc], with [side_color] (if supplied) *)
let print_property_color ?(side_color = ANSITerminal.on_white) 
    (color : ANSITerminal.style) (loc : int * int) : unit =
  print_in_place ~b_color:side_color ["│"] loc;
  print_in_place ~b_color:color ["                "] (fst loc + 1, snd loc);
  print_in_place ~b_color:side_color ["│"] (fst loc + 17, snd loc)

(** [print_side color loc] prints the box-building characters of a property's
    sides at location [loc], with where the sides have a [color] *)
let print_sides (color : ANSITerminal.style) (loc : int * int) : unit =
  print_in_place ~b_color:color ["│"] (fst loc, snd loc + 1);
  print_in_place ~b_color:color ["│"] (fst loc + 17, snd loc + 1);
  print_in_place ~b_color:color ["│"] (fst loc, snd loc + 2);
  print_in_place ~b_color:color ["│"] (fst loc + 17, snd loc + 2);
  print_in_place ~b_color:color ["│"] (fst loc, snd loc + 3);
  print_in_place ~b_color:color ["│"] (fst loc + 17, snd loc + 3)

(** [print_property name loc p_type] prints the property with [name] and 
    [p_type] to location [loc]. If [color] is supplied, the property will 
    be printed with that [color]. If [highlight] is supplied and [highlighter]
    is true, then the sides of that property will be highlighted with color
    [highlight] *)
let print_property ?(color = ANSITerminal.on_default) 
    ?(highlight = ANSITerminal.on_default) ?(highlighter = false) 
    (name : string * string) (loc : int * int) (p_type : p_type) : unit =
  if highlighter then
    print_sides highlight loc
  else 
    (print_property_color ~side_color:highlight color (fst loc, snd loc + 1);
     produce_property_text ~side_color:highlight (fst name) 
       (fst loc, snd loc + 2);
     produce_property_text ~side_color:highlight (snd name) 
       (fst loc, snd loc + 3));
  match p_type with
  | Horiz -> 
    print_in_place ~b_color:highlight ["┬────────────────┬"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["┴────────────────┴"] 
      (fst loc, snd loc + 4);
  | Vert -> 
    print_in_place ~b_color:highlight ["├────────────────┤"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["├────────────────┤"] 
      (fst loc, snd loc + 4);
  | TLeft -> 
    print_in_place ~b_color:highlight ["┌────────────────┬"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["├────────────────┼"] 
      (fst loc, snd loc + 4);
  | TRight -> 
    print_in_place ~b_color:highlight ["┬────────────────┐"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["┼────────────────┤"] 
      (fst loc, snd loc + 4);
  | BLeft -> 
    print_in_place ~b_color:highlight ["├────────────────┼"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["└────────────────┴"] 
      (fst loc, snd loc + 4);
  | BRight -> 
    print_in_place ~b_color:highlight ["┼────────────────┤"] 
      (fst loc, snd loc);
    print_in_place ~b_color:highlight ["┴────────────────┘"] 
      (fst loc, snd loc + 4)

(** [highlight_property id color game] highlights the property with tile_id
    [id] in the [game] with [color] *)
let highlight_property (id : int) (color : ANSITerminal.style) 
    (game : Game.t): unit =
  print_property ~color:(get_tile_color id game) ~highlight:color 
    ~highlighter:true (get_tile_name id game) (get_tile_loc id) 
    (get_tile_type id)

(** [unhighlight_property id game] unhighlights the property with tile_id
    [id] in the [game]. In other words, the colors of the box-building 
    characters of the property are set back to white. *)
let unhighlight_property (id : int) (game : Game.t) : unit = 
  print_property ~color:(get_tile_color id game) 
    ~highlight:ANSITerminal.on_default 
    ~highlighter:true (get_tile_name id game) (get_tile_loc id) 
    (get_tile_type id)

(** [print_dice num loc] prints a dice with [num] dots at location [loc] *)
let print_dice (num : int) (loc : int * int) : unit =
  match num with
  | 0 ->
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│       │"] (fst loc, snd loc + 1);
    print_in_place ["│       │"] (fst loc, snd loc + 2);
    print_in_place ["│       │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 1 -> 
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│       │"] (fst loc, snd loc + 1);
    print_in_place ["│   •   │"] (fst loc, snd loc + 2);
    print_in_place ["│       │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 2 ->
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│ •     │"] (fst loc, snd loc + 1);
    print_in_place ["│       │"] (fst loc, snd loc + 2);
    print_in_place ["│     • │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 3 -> 
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│ •     │"] (fst loc, snd loc + 1);
    print_in_place ["│   •   │"] (fst loc, snd loc + 2);
    print_in_place ["│     • │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 4 -> 
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 1);
    print_in_place ["│       │"] (fst loc, snd loc + 2);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 5 -> 
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 1);
    print_in_place ["│   •   │"] (fst loc, snd loc + 2);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | 6 -> 
    print_in_place ["╭───────╮"] (fst loc, snd loc);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 1);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 2);
    print_in_place ["│ •   • │"] (fst loc, snd loc + 3);
    print_in_place ["╰───────╯"] (fst loc, snd loc + 4);
  | _ -> ()


let animate_dice ?(time_duration = 0.5) (dice1 : int) (dice2 : int) : unit =
  let rec helper current_time prev_ftime time_duration =
    let time = Unix.gettimeofday () in
    if time_duration <= 0. then 
      (print_dice (dice1) dice; 
       print_dice (dice2) (fst dice + dice_spacing, snd dice)) 
    else if current_time -. prev_ftime >= dice_delay then
      (print_dice (Random.int 6 + 1) dice;
       print_dice (Random.int 6 + 1) (fst dice + dice_spacing, snd dice);
       let diff = time -. current_time in
       helper time time (time_duration -. diff))
    else helper time prev_ftime (time_duration -. time +. current_time) in
  helper (Unix.gettimeofday ()) ((Unix.gettimeofday ()) -. dice_delay)
    time_duration

(** [disp_go_message] informs the user that they have passed go *)
let disp_go_message () : unit =
  print_banner "You received $200 for passing \"Go\"" 1 ANSITerminal.on_default

(** [rotate_highligh start_id end_id color game] animates a player moving
    around the board, successively highlighting and unhighlighting all of 
    the properties between tile_ids [start_id] and [end_id] [color] in the 
    [game] *)
let rotate_highlight (start_id : int) (end_id : int) 
    (color : ANSITerminal.style) (game : Game.t) : unit =
  let end_id = if end_id < start_id then (end_id + 40) else end_id in
  let rec helper start_id end_id = 
    if start_id + 1 <= end_id then
      (unhighlight_property (start_id mod 40) game;
       highlight_property ((start_id + 1) mod 40) color game;
       (if start_id + 1 >= 40 then
          disp_go_message () else ());
       Unix.sleepf 0.1;
       helper (start_id + 1) end_id)
    else () in
  helper start_id end_id


let animate_card_move (start_id : int) (end_id : int) (player_id : int)
    (game : Game.t) : unit = 
  rotate_highlight start_id end_id (get_player_color player_id) game

(** [print_console_line] prints a line across the bottom of the console *)
let print_console_line () : unit = 
  print_in_place ["╪════════════════════════════════════════════════════════"
                  ^ "═══════════════════════════════════════════════════════"
                  ^ "═════════════════════════════════════════╪"] 
    (fst console - 2, (snd console) + console_height + 1)

(** [print_board game] prints the initial state of the board to the screen in 
    the [game] *)
let print_board (game : Game.t) : unit = 
  ANSITerminal.erase ANSITerminal.Screen;
  let rec helper = function
    | [] -> ()
    | h::t -> 
      let num = fst h in
      print_property ~color:(get_tile_color num game) (get_tile_name num game) 
        (snd h) (get_tile_type num);
      helper t
  in helper tile_locations;
  print_console_line ()

(** [format_player string] pads the [string], representing a player name, with
    trailing whitespace until the resulting string has length 9. Useful for
    printing to the screen in specified locations. *)
let format_player (player : string) : string =
  player ^ String.make (9 - String.length player) ' '

(** [dice_message dice1 dice2] is the string representing the message displayed
    when the player rolls a pair of dice with values [dice1] and [dice2] *)
let dice_message (dice1:int) (dice2:int) : string =
  "Threw a " ^ (string_of_int dice1) ^ " and a " ^ (string_of_int dice2) ^ 
  " (" ^ (string_of_int (dice1 + dice2)) ^ ")"

(** [roll_message player dice1 dice2] is the same as [dice_message dice1 dice2],
    with the [player] name prepended to the message. *)
let roll_message (player : string) (dice1 : int) (dice2 : int) : string =
  player ^ ", you threw a " ^ (string_of_int dice1) ^ " & " ^ 
  (string_of_int dice2)


let disp_menu highlight_line = 
  menu_banner                             "╔═ Main Menu═<SPACE> to select═╗" 
    0 menu_width;
  menu_banner ~highlight:(highlight_line = 0) ("║     Mortgage / Unmortgage"^
                                               "    ║") 1 menu_width;
  menu_banner ~highlight:(highlight_line = 1) ("║     Buy House             "^
                                               "   ║") 2 menu_width;
  menu_banner ~highlight:(highlight_line = 2) ("║     Sell House             "^
                                               "  ║") 3 menu_width;
  menu_banner                                 ("║ ────────────────────────"^
                                               "──── ║") 4 menu_width;
  menu_banner ~highlight:(highlight_line = 3) ("║     Resign From Game   "^
                                               "      ║") 5 menu_width;
  menu_banner                                 ("║ ─────────────────────"^
                                               "─────── ║") 6 menu_width;
  menu_banner ~highlight:(highlight_line = 4) ("║     Save Game          "^
                                               "      ║") 7 menu_width;
  menu_banner                                 ("║ ───────────────────────"^
                                               "───── ║") 8 menu_width;
  menu_banner ~highlight:(highlight_line = 5) ("║     Exit game, return t"^
                                               "o O/S ║") 9 menu_width;
  menu_banner                                 ("╚═<W/S> to move <ESC> To "^
                                               "Exit ═╝") 10 menu_width;
  typeable ()


let raise_cash_menu (highlight_line : int) : unit = 
  menu_banner                                 "╔══<SPACE> to select══╗" 
    2 cash_menu_width;
  menu_banner ~highlight:(highlight_line = 0) "║     Mortgage        ║" 
    3 cash_menu_width;
  menu_banner ~highlight:(highlight_line = 1) "║     Sell House      ║" 
    4 cash_menu_width;
  menu_banner ~highlight:(highlight_line = 2) "║     Resign          ║" 
    5 cash_menu_width;
  menu_banner ~highlight:(highlight_line = 3) "║     Quit Menu       ║" 
    6 cash_menu_width;
  menu_banner                                 "╚══<W/S> to move══════╝" 
    7 cash_menu_width


let clear_cash_menu () : unit =
  clear_console ()


let print_out_of_cash_message (amt_owed : int) (amt_needed : int) : unit =
  let message = 
    if amt_needed <= 0 
    then "You owe $" ^ (string_of_int amt_owed) ^ ", and can now afford it"
    else "You owe $" ^ (string_of_int amt_owed) ^ ", and need another $"
         ^ (string_of_int amt_needed) in
  print_banner message 1 ANSITerminal.on_default


let clear_menu () : unit = 
  clear_console ();
  clear_line 11;
  clear_line 12;
  clear_line 13;
  clear_line 14;
  print_console_line ()

(** [clear_property_console] clear the property console from the screen *)
let clear_property_console () =
  clear_line 12;
  clear_line 13;
  clear_line 14;
  clear_line 15;
  clear_line 16;
  clear_line 17;
  clear_line 18;
  clear_line 19;
  clear_line 20


let disp_player_info (player_list : (string * int) list) : unit =
  clear_line 40;
  clear_line 41;
  clear_line 42;
  let rec helper player_list i =
    match player_list with
    | [] -> ()
    | (name, money)::t ->
      let loc = get_print_player_info_loc i in
      let player_string = "Player " ^ (string_of_int (i + 1)) ^ " : " in
      print_in_place [player_string] loc;
      print_in_place ~b_color:(get_player_color i) [format_player name] 
        (fst loc + 11, snd loc);
      print_in_place [" Money : " ^ string_of_int money] 
        (fst loc + 20, snd loc);
      helper t (i+1)
  in helper player_list 0

(** [property_string property_name mortgage_value] is a string formatted for 
    printing to the property console, with tha name [property_name] and 
    mortage value [mortgage_value] *)
let property_string property_name mortgage_value = 
  let mortgage = string_of_int mortgage_value in
  let pre = "█ " ^ (center 23 (String.sub (property_name) 0 
                                   (min (String.length property_name) 23))) ^
            " █ " in
  let mortgage = if String.length mortgage = 2 then " " ^ mortgage else 
      mortgage in pre ^ mortgage ^ " █" 


let print_player_properties (prop_names : (int * int * bool * int) list) 
    (game : Game.t): unit =
  clear_property_console ();
  let rec helper prop_names game counter =
    match prop_names with
    | [] -> ()
    | (prop_id, mort_value, mortgaged, num_houses)::t ->
      let tile_tup = get_tile_name prop_id game in
      let tile_name = fst tile_tup ^ " " ^ snd tile_tup in
      let x_loc = if counter mod 3 = 0 then 20 else if counter mod 3 = 1 then 60
        else 100 in
      let y_loc = counter / 3 in
      if mortgaged then
        (print_to_property_console ~b_color:ANSITerminal.on_red 
           (property_string tile_name mort_value) (x_loc,y_loc) 
           (ANSITerminal.black);
         helper t game (counter + 1))
      else
        (print_to_property_console (property_string tile_name mort_value) 
           (x_loc,y_loc) (get_tile_foreground_color prop_id game);
         print_to_property_console (string_of_int num_houses) 
           (x_loc + 32, y_loc) ANSITerminal.green;
         helper t game (counter + 1)) in
  helper prop_names game 0


let print_property_prompts 
    (mapping : (int * string) list) 
    (game : Game.t): unit =
  let rec helper mapping game counter =
    match mapping with
    | [] -> ()
    | (_, char)::t ->
      let x_loc = if counter mod 3 = 0 then 20 else if counter mod 3 = 1 then 60
        else 100 in
      let y_loc = counter / 3 in
      print_to_property_console char (x_loc - 2 ,y_loc) ANSITerminal.on_default;
      helper t game (counter + 1) in
  helper mapping game 0


let print_house_message () : unit =
  print_banner ("Press the letter corresponding to the property to buy a " ^
                "house on it") 3 ANSITerminal.on_default;
  print_banner "Press <ESC> to end" 4 ANSITerminal.on_default;
  print_banner "Costs:" 6 ANSITerminal.on_default;
  print_to_console ~f_color:ANSITerminal.white   "█" (50,7);
  print_to_console ~f_color:ANSITerminal.red     "█" (50,8);
  print_to_console ~f_color:ANSITerminal.cyan    "█" (54,7);
  print_to_console ~f_color:ANSITerminal.yellow  "█" (54,8);
  print_to_console ~f_color:ANSITerminal.magenta "█" (83,7);
  print_to_console ~f_color:ANSITerminal.black   "█" (87,7);
  print_to_console ~f_color:ANSITerminal.green   "█" (83,8);
  print_to_console ~f_color:ANSITerminal.blue    "█" (87,8);
  print_to_console "&" (52,7);
  print_to_console "&" (52,8);
  print_to_console "&" (85,7);
  print_to_console "&" (85,8);
  print_to_console "- $ 50 each" (56,7);
  print_to_console "- $150 each" (56,8);
  print_to_console "- $100 each" (89, 7);
  print_to_console "- $200 each" (89, 8)


let determine_start_player 
    (players : (string * (int * int)) list) 
    (first_player : string) 
    (index : int) 
    (state : State.t)
    (game : Game.t) : unit = 
  print_board game;
  let rec initial_cash_map l =
    match l with
    | [] -> []
    |(name, _)::t -> (name, 1500)::(initial_cash_map t) in
  disp_player_info (initial_cash_map players);
  print_dice 0 dice; print_dice 0 (fst dice + dice_spacing, snd dice);
  print_to_console ~b_color:ANSITerminal.on_red 
    "Dice Throws To See Who Moves First" (0,0);
  let rec print_players (players : (string * (int * int)) list) index =
    match players with
    | [] -> ()
    | (name, _)::t ->
      print_to_console ~b_color:(List.nth player_colors index) 
        (format_player (name ^ ":")) (0, index + 2);
      print_players t (index + 1) in
  let rec roll_players (players : (string * (int * int)) list) index =
    match players with
    | [] -> ()
    | (name, (roll1, roll2))::t -> 
      print_to_console "Press any key to throw the dice" (10, index + 2);
      typeable ();
      ignore (get1char ~player_name:name state Continue);
      animate_dice roll1 roll2;
      clear_line (index + 2);
      print_to_console ~b_color:(List.nth player_colors index) 
        (format_player (name ^ ":")) (0, index + 2);
      print_to_console (dice_message roll1 roll2) (10, index + 2);
      roll_players t (index + 1)
  in print_players players 0;
  roll_players players 0;
  print_banner (first_player ^ " will start the game") 
    1 ANSITerminal.on_default;
  print_banner "Press any key to continue" 9 ANSITerminal.on_red;
  highlight_property 0 (get_player_color index) game;
  typeable ();
  ignore (get1char state Continue)


let buy_property 
    (property_id : int) 
    (player_index : int) 
    (game : Game.t) : unit =
  let color = get_player_color player_index in
  let loc = get_tile_loc property_id in
  let name = get_tile_name property_id game in
  print_in_place ~b_color:color [center property_width (fst name)] 
    (fst loc + 1, snd loc + 2);
  print_in_place ~b_color:color [center property_width (snd name)] 
    (fst loc + 1, snd loc + 3)


let redistribute_property (player_property : int * Game.tile_id list) 
    (state : State.t) (game : Game.t): unit = 
  print_banner "You don't have enough money for that! You lose!" 
    5 ANSITerminal.on_red;
  let rec helper player_property = 
    match player_property with
    | _, [] -> ()
    | player_index, property_id::t -> 
      let color = ANSITerminal.on_default in
      let loc = get_tile_loc property_id in
      let name = get_tile_name property_id game in
      print_in_place ~b_color:color [center property_width (fst name)] 
        (fst loc + 1, snd loc + 2);
      print_in_place ~b_color:color [center property_width (snd name)] 
        (fst loc + 1, snd loc + 3);
      helper (player_index,t)
  in helper player_property;
  unhighlight_property (State.current_player_tile_id state) game;
  typeable ();
  ignore (get1char state Continue)

(** [house_text num_houses] is the text representing houses on a property 
    with [num_houses] *)
let house_text num_houses = 
  if num_houses = 0 then ["                "] else
  if num_houses = 1 then ["       *        "] else
  if num_houses = 2 then ["       **       "] else
  if num_houses = 3 then ["       ***      "] else
  if num_houses = 4 then ["       ****     "] else
  if num_houses = 5 then ["      Hotel     "] else
    failwith "Invalid number of houses"

(** [print_property_color color num_houses loc] prints out the [color] of a 
    property located at [loc], with [num_houses] *)
let print_house  
    (color : ANSITerminal.style) (num_houses : int) (loc : int * int) : unit =
  print_in_place ~b_color:color ~f_color:ANSITerminal.on_green 
    (house_text num_houses) (fst loc + 1, snd loc + 1)


let disp_buy_house (id : Game.tile_id) (num_houses : int) 
    (game : Game.t) : unit =
  let tile_color = get_tile_color id game in
  let loc = get_tile_loc id in
  print_house tile_color num_houses loc


let print_board_from_file (owners : (int * Game.tile_id) list) 
    (current_player : int) (current_tile : Game.tile_id) 
    (players_cash : (string * int) list)
    (tile_houses : (Game.tile_id * int) list)
    (game : Game.t) : unit = 
  print_board game;
  let rec helper owners =
    match owners with
    | [] -> ()
    | (player, tile)::t -> 
      buy_property tile player game;
      helper t in
  helper owners;
  highlight_property current_tile (get_player_color current_player) game;
  let rec print_houses tile_houses = 
    match tile_houses with
    | [] -> ()
    | (tile_id, num_houses)::t -> 
      disp_buy_house tile_id num_houses game;
      print_houses t in
  print_houses tile_houses;
  disp_player_info players_cash


let disp_turn (current_player : string) (player_number : int) 
    (current_tile : string) : unit =
  clear_console ();
  print_console_line ();
  print_banner "<SPACE> to throw dice, <ESC> for menu" 0 ANSITerminal.on_red;
  print_banner ("It is " ^ current_player ^ "'s turn") 27 
    (get_player_color player_number);
  print_to_console ("You are on: " ^ current_tile) (35, 10);
  typeable ()


let roll_dice (current_player :string) (index : int) (dice : int * int) 
    (old_loc : int) (game : Game.t) : unit =
  let new_loc = (old_loc + fst dice + snd dice) mod 40 in
  let next_tile_name = get_tile_name new_loc game in
  print_banner "The dice are spinning" 0 ANSITerminal.on_red;
  animate_dice (fst dice) (snd dice);
  print_banner (roll_message current_player (fst dice) (snd dice)) 
    0 ANSITerminal.on_default;
  rotate_highlight old_loc new_loc (get_player_color index) game;
  clear_line 10;
  print_to_console ("You are on: " ^ (fst next_tile_name) ^ " " ^ 
                    (snd next_tile_name)) (35, 10);
  typeable ()


let disp_buy_message (property_id : int) (game : Game.t) : unit =
  let name = get_tile_name property_id game in
  let price = get_property_by_id property_id game |> sale_price in
  print_banner ((fst name) ^ " " ^ (snd name) ^ " is for sale,") 
    3 ANSITerminal.on_default;
  print_banner ("Do you want to buy it for $" ^ (string_of_int price) ^ "? ") 
    5 ANSITerminal.on_default


let confirm_buy (state : State.t) : unit = 
  clear_line 0;
  print_banner ("You bought it") 7 ANSITerminal.on_default;
  print_banner ("Press any key for next players turn") 9 ANSITerminal.on_red;
  typeable ();
  ignore (get1char state Continue)


let deny_buy (state : State.t) = 
  clear_line 0;
  print_banner ("You didn't buy it") 7 ANSITerminal.on_default;
  print_banner ("Press any key for next players turn") 9 ANSITerminal.on_red;
  typeable ();
  ignore (get1char state Continue)


let disp_change_turn (old_tile : int) (new_player : int) (new_tile : int) 
    (game : Game.t) : unit = 
  unhighlight_property old_tile game;
  highlight_property new_tile (get_player_color new_player) game

(** [roll_utility roll] animates rolling dice for a given [roll] when landing
    on a property which is a utility *)
let roll_utility (roll : int * int) =
  print_banner "The dice are spinning" 0 ANSITerminal.on_red;
  animate_dice (fst roll) (snd roll);
  print_banner ("You threw a " ^ (string_of_int (fst roll)) ^ " & " ^ 
                (string_of_int (snd roll))) 0 ANSITerminal.on_default


let disp_rent_message 
    ?(is_utility = false) 
    ?(roll = 0,0) 
    (prop_name : string)
    (owner_name : string) 
    (rent : int)
    (state : State.t) : unit = 
  print_banner (prop_name ^ " belongs to " ^ owner_name) 
    3 ANSITerminal.on_default;
  if is_utility then
    (print_banner "Press any key to roll the dice and determine amount owed" 
       5 ANSITerminal.on_red;
     typeable ();
     ignore (get1char state Continue);
     roll_utility roll) else ();
  print_banner ("You owe $" ^ string_of_int rent) 5 ANSITerminal.on_default;
  print_banner ~text_color:ANSITerminal.black "Press any key to pay rent" 
    9 ANSITerminal.on_yellow;
  typeable ();
  ignore (get1char state Continue)


let disp_chance_message () = 
  print_banner "Press any key to pick up a \"Chance\" card." 
    5 ANSITerminal.on_default


let disp_tax_message (amt : int) (state : State.t) =
  print_banner ("Press any key to pay tax of $" ^ (string_of_int amt)) 
    5 ANSITerminal.on_default;
  ignore (get1char state Continue)


let disp_go_to_jail () : unit = 
  print_banner "You have been sent to jail!" 5 ANSITerminal.on_magenta


let disp_jail_doubles () : unit = 
  print_banner ("You have been sent to jail for rolling three" ^ 
                " consecutive doubles!") 5 ANSITerminal.on_magenta


let disp_jail_fine () : unit =
  print_banner ("Press <SPACE> to pay a $50 jail fine.") 
    5 ANSITerminal.on_default


let disp_stuck_in_jail () : unit =
  print_banner ("You failed to roll a double, so you're stuck in jail.")
    5 ANSITerminal.on_default


let animate_jail player_index current_tile game state = 
  unhighlight_property current_tile game;
  highlight_property 10 (get_player_color player_index) game;
  let tile_name = get_tile_name 10 game in
  clear_line 0;
  print_banner ("Press any key for next players turn") 9 ANSITerminal.on_red;
  clear_line 10;
  print_to_console ("You are on: " ^ (fst tile_name) ^ " " ^ 
                    (snd tile_name)) (35, 10);
  typeable ();
  ignore (get1char state Continue)


let disp_rolled_double (state : State.t) : unit = 
  print_banner ("You rolled a double! Press any key to continue your turn.") 
    9 ANSITerminal.on_red;
  ignore (get1char state Continue)


let pause_message (state : State.t) : unit =
  print_banner ("Press any key for next players turn") 9 ANSITerminal.on_red;
  ignore (get1char state Continue)


let mortgage_message () : unit =
  print_banner "Press letter corresponding to property to (un)mortgage" 4 
    ANSITerminal.on_default;
  print_banner "Press <ESC> to end" 7 ANSITerminal.on_default


let disp_monopoly_message () : unit =
  print_banner "You need a monopoly to buy houses!" 10 ANSITerminal.on_default


let disp_too_many_houses () : unit = 
  print_banner "You have the max number of houses already!" 
    10 ANSITerminal.on_default


let disp_invalid_mortgage () : unit =
  print_banner "You cannot mortgage that property" 10 ANSITerminal.on_default


let disp_no_houses () : unit =
  print_banner "You have no houses to sell!" 10 ANSITerminal.on_default


let disp_no_railroad_house () : unit = 
  print_banner "Bruh you can't put a house on a railroad!" 
    10 ANSITerminal.on_default


let win_message (st : State.t) : unit =
  let name = State.current_player_name st in
  print_banner ("Everyone else has resigned, " ^ name ^ " wins the game!") 
    9 ANSITerminal.on_green;
  ignore (get1char st Continue);
  ANSITerminal.erase ANSITerminal.Screen;
  exit 0


let card_message (msg : string) (st : State.t) : unit =
  print_banner (msg ^ " Press any key to continue.") 5 ANSITerminal.blue;
  ignore (get1char st Continue);
  clear_line 5


let forced_payment (amt : int) (st : State.t) : unit =
  print_banner ("You paid $" ^ string_of_int amt ^ 
                ". Press any key to continue") 5 ANSITerminal.red;
  ignore (get1char st Continue);
  clear_line 5