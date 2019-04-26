exception Empty

type command = 
  | Roll
  | RollDouble
  | Menu
  | Mortgage
  | Buy
  | Sell
  | Resign
  | Save
  | Exit  
  | Close
  | Malformed

type choice =
  | Yes
  | No
  | ChoiceMalformed

let make_list_of_words (s : string) : string list = 
  String.trim s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")

let parse (character : char) : command = 
  match character with 
  | ' ' -> Roll
  | 'd' -> RollDouble
  | '\027' -> Menu
  | _ -> Malformed

let parse_confirm (character : char) = 
  match character with
  | 'y' -> Yes
  | 'n' -> No
  | _ -> ChoiceMalformed

let parse_menu_option (menu_num : int) : command =
  match menu_num with
  | 0 -> Mortgage
  | 1 -> Buy
  | 2 -> Sell
  | 3 -> Resign
  | 4 -> Save
  | 5 -> Exit
  | 6 -> Close
  | _ -> Malformed

let parse_cash_menu_option (menu_num : int) : command =
  match menu_num with
  | 0 -> Mortgage
  | 1 -> Sell
  | 2 -> Resign
  | 3 -> Close
  | _ -> Malformed