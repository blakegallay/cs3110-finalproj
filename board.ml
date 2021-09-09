(** [Board.ml] has all of the functionality for the board and players. *)

exception Invalid_Player 
exception Invalid_Tile
exception Invalid_Board

type direction = Forwards | Backwards

type game = Empty | Game of Minigames.minigame

type item = Teleporter | Bomb | Deed of string

type structure = PlantedBomb | Property of string * string

type store = Empty_Store | Store of (item*int) list | Star_Store 

type tile =
  { 
    number : int;
    game : game;
    next : int list;
    structures : structure list;
    store : store;
    tile_coins : int
  }

type player = 
  {
    name : string;
    id: int;
    tile : int;
    in_move_queue : bool;
    inventory: item list;
    coins: int;
    star_count: int;
  }

type board = tile list

(*****************************************************************************)
(** The following deal with all of the items and structures on
    the board. *)

(** [get_random_deed x] is a random deed. *)
let get_random_deed x = 
  let props = ["ISS"; "Endurance"; "Enterprise"; "Falcon"; "Boardwalk"] in 
  List.nth props (Random.int (List.length props))

let find_player name players = List.find (fun p -> p.name = name) players 

let players_on_tile players tile = 
  List.filter (fun p -> p.tile = tile.number) players

(* [transfer_coins' giver receiver players to_transfer] is a helper for
   [transfer_coins]. *)
let transfer_coins' giver receiver players to_transfer =
  List.map (fun p -> if p.name = giver then 
               {
                 p with 
                 coins=p.coins - to_transfer;
               } else if p.name = receiver then 
               {
                 p with
                 coins=p.coins + to_transfer;
               } else p) players

let transfer_coins giver receiver players amount = 
  let to_transfer = 
    let giver_balance = (find_player giver players).coins in 
    if giver_balance >= amount then amount 
    else giver_balance 
  in 
  print_endline (String.concat "" [giver; " paid "; receiver; " "; 
                                   string_of_int to_transfer; " coins!"]);
  transfer_coins' giver receiver players to_transfer

let rec find_tile tile board = 
  match board with 
  | [] -> print_endline "???"; raise Invalid_Tile
  | a::b -> if a.number = tile then a else find_tile tile b

(** [use_teleporter' p user players board] is a helper for [use_teleporter]. *)
let use_teleporter' p user players board = 
  List.map (
    fun e -> if e = user then 
        {
          e with 
          tile=p.tile;
          inventory=List.filter 
              (fun i -> match i with |Teleporter -> false | _ -> true) 
              e.inventory;
        }else if e = p then 
        {
          e with
          tile=user.tile;
        }
      else e) players, board, true

(**TODO: *)
let rec use_teleporter user players board = 
  let _ = List.map 
      (fun p -> if p = user then () else 
          print_endline (String.concat "" [p.name; " : Tile #"; 
                                           string_of_int p.tile; "\n"])) players
  in 
  print_endline "Enter a player's name to switch places with them, or 'cancel' \
  to go back.";
  print_string ">";
  match read_line () with 
  | "cancel" -> (players, board, false)
  | s -> match List.find_opt (fun p -> p.name = s) players with 
    | Some p -> print_endline (String.concat "" [
        "The teleporter whirs to life, and you instantly warp to tile #"; 
        string_of_int p.tile; "!\n";
        p.name; " takes your place at tile #"; string_of_int user.tile; "\n"]); 
      use_teleporter' p user players board
    | None -> print_endline "That's not a valid player."; 
      use_teleporter user players board 


(**[use_bomb'] is a helper for use_bomb. *)
let use_bomb' user players board = 
  ( List.map (fun p -> if p = user then  
                 {
                   p with
                   inventory=List.filter 
                       (fun i -> match i with |Bomb -> false | _ -> true) 
                       p.inventory;
                 } else p) players, 
    List.map (fun t -> if t = (find_tile user.tile board) then 
                 {
                   t with
                   structures=PlantedBomb::t.structures;
                 } else t) board, 
    true )

(**TODO: *)
let rec use_bomb user players board = 
  print_string ">";
  match read_line () with 
  | "cancel" -> (players, board, false)
  | "plant" -> print_endline (String.concat "" ["You planted the bomb. The next\
   player to traverse tile #"; string_of_int user.tile; " will have their turn\
    ended immediately!\n"]); 
    use_bomb' user players board
  | _ -> print_endline "I don't recognize that input."; 
    use_bomb user players board

(** [use_deed' user players board deed_name] is a helper for [use_deed]. *)
let use_deed' user players board deed_name = 
  List.map (fun p -> if p = user then 
               { 
                 p with
                 inventory = List.filter 
                     (fun i -> 
                        match i with |Deed deed_name -> false | _ -> true)
                     p.inventory;
               } else p) players, 
  List.map (fun t -> if t = (find_tile user.tile board) then 
               {
                 t with 
                 structures = (Property (deed_name,user.name))::t.structures; 
               } else t) board, 
  true

(** TODO:*)
let use_deed user players board deed_name = 
  print_string ">";
  match read_line () with 
  | "cancel" -> (players, board, false)
  | "sign" -> print_endline (String.concat "" ["You claimed the "; deed_name; "\
   on tile #"; string_of_int user.tile; ".\nFor the rest of the game, any \
   player who lands on this tile has to pay a 5 coin property tax!\n"]);
    ( 
      use_deed' user players board deed_name
    )
  | _ -> print_endline "I don't recognize that input."; 
    use_bomb user players board 

let use_item user item players board : (player list * board * bool) = 
  match item with 
  | Teleporter ->   print_endline (
      "\n~~~~~~~~~~~~~~\n Teleporter \n~~~~~~~~~~~~~~\nThis device lets you \
      instantly swap places with another player on the board. \nHere are all \
      of the other players and their locations:\n"
    ); 
    use_teleporter user players board
  | Bomb ->  if (List.mem PlantedBomb (find_tile user.tile board).structures) 
    then (print_endline "There's already a bomb on the current tile, so another\
     can't be planted.\n"; (players, board, false)) 

    else (print_endline (
        String.concat "" ["\n~~~~~~~~~~~~~~\n Bomb \n~~~~~~~~~~~~~~\nThis \
        dangerous plasma bomb explodes on contact and will stop any player in \
        their tracks! \n Enter 'plant' to plant the bomb on the current tile \
        (Tile #";string_of_int user.tile; "), or 'cancel' to go back.\n"]
      ); 
       use_bomb user players board) 
  | Deed s -> if not (List.filter 
                        (fun s -> match s with |Property _ -> true | _ -> false)
                        (find_tile user.tile board).structures = []) then 

      (print_endline "There's already a claimed property on this tile, so \
      another can't be claimed.\n"; (players, board, false)) else (
      print_endline ( 
        String.concat ""
          ["\n~~~~~~~~~~~~~~\n Deed \n~~~~~~~~~~~~~~\nThis document, when \
          signed, will transfer ownership of the "; s; " on tile #";
           string_of_int user.tile; " to you.\nType 'sign' to sign the deed, or\
            'cancel' to go back.\n"]
      ); 
      use_deed user players board s )

let string_of_item it = match it with 
  | Teleporter -> "Teleporter"
  | Bomb -> "Bomb"
  | Deed s -> String.concat "" ["Deed to the "; s]

let string_of_structure st = match st with 
  | PlantedBomb -> "Land Mine"
  | Property (s,o) -> String.concat "" [o; "'s "; s]

let remove_structure st tle board = List.map (fun t -> 
    if t = tle then {
      t with
      structures= List.filter (fun s -> not (s = st)) t.structures;
    } else t) board 

(*****************************************************************************)
(** The following deal with all of the functionality of the board. *)

(** [construct_next current_tile] is a list of the tiles accessible from
    [current_tile]. *)
let construct_next current_tile = match current_tile with
    1 -> [2;15]
  |2 -> [1;3;4]
  |3 -> [2;5]
  |4 -> [2;6]
  |5 -> [3;7]
  |6 -> [4;8]
  |7 -> [5;10;11]
  |8 -> [6;11;9]
  |9 -> [8;12]
  |10 -> [7;13]
  |11 -> [7;8;13]
  |12 -> [9;14]
  |13 -> [10;11;12;14]
  |14 -> [12;13;15]
  |15 ->[1;14]
  |_ -> raise Invalid_Tile

(** [construct_coins current_tile] is the number of coins on [current_tile]. *)
let construct_coins current_tile =
  if current_tile = 1 || current_tile = 4 || current_tile = 11 || 
     current_tile = 14 then 1
  else if current_tile = 2 || current_tile = 7 || current_tile = 13 then 2
  else 3

(** [construct_minigames] is the minigame field for [current_tile]. *)
let rec construct_minigames current_tile game_tiles =
  match game_tiles with 
  | [] -> Empty
  |(a,b)::c -> if a = current_tile then Game b 
    else construct_minigames current_tile c

(** [construct_stores current_tile store_tiles] is the store field for 
    current_tile. *)
let rec construct_stores current_tile store_tiles =
  match store_tiles with 
  | [] -> Empty_Store
  |(a,b)::c -> if a = current_tile then Store b 
    else construct_stores current_tile c

(** [construct_board' current_tile store_tiles game_tiles lst] makes a board 
    [lst] of 10 tiles. *)
let rec construct_board' current_tile store_tiles game_tiles lst =
  if current_tile > 15 then lst
  else 
    let tile = 
      {
        number = current_tile; 
        game = construct_minigames current_tile game_tiles;
        next = construct_next current_tile;
        structures = [];
        store = if current_tile = 15 then Star_Store else 
            construct_stores current_tile store_tiles; 
        tile_coins = construct_coins current_tile 
      } in 
    construct_board' (current_tile+1) store_tiles game_tiles (tile::lst)

(** [randomize_stores lst objects amount size games] constructs an (int*object) 
    assoc [lst] of [size] elements with entires corresponding to the tile 
    numbers associated with each object. Tile numbers cannot be the same as 
    those in [games]. *)
let rec randomize_stores lst objects amount size games =
  if List.length lst >= amount then lst 
  else 
    let number = (Random.int size-2) + 2 in
    if List.mem_assoc number games then randomize_stores lst objects amount 
        size games
    else match objects with 
      | [] -> failwith "No objects"
      | a::b -> let lst' = ((number,a)::lst) in randomize_stores lst' b amount 
          size games

(** [randomize_minigames lst objects amount size] constructs an (int*object) 
    assoc [lst] of [size] elements with entires corresponding to the tile
    numbers associated with each object. *)
let rec randomize_minigames lst objects amount size =
  if List.length lst >= amount then lst else 
    let number = (Random.int size-2)+2 in 
    if List.mem_assoc number lst then 
      randomize_minigames lst objects amount size 
    else match objects with 
      | [] -> failwith "No objects"
      | a::b -> let lst' = ((number,a)::lst) in randomize_minigames lst' b 
          amount size

(** [stores'] is a list of the stores of the game. *)
let stores' = let store1 = [(Teleporter,2); (Bomb,3); ] in 
  let store2 = [(Bomb,1); (Deed (get_random_deed ()),2)] in  
  let store3 = [(Teleporter, 4); (Deed (get_random_deed()),1)] 
  in [store1;store2;store3]

let construct_board the_games : board = 
  let stores = stores' in 
  let game_tiles = randomize_minigames [] the_games (List.length the_games) 15
  in 
  let store_tiles = randomize_stores [] stores (List.length stores) 15 
      game_tiles in
  List.rev(construct_board' 1 store_tiles game_tiles [])

(** [next_tile' current direction board options lst] is all of the tiles
    the player can move to from [current] depending on their [direction]. *)
let rec next_tile' current direction board options lst = 
  match board with 
  | [] -> lst
  | a::b -> if List.mem a.number options then 
      check_direction current a b direction options lst
    else next_tile' current direction b options lst

(** [check_direction current a b direction options lst] is the next possible
    tiles after checking the [direction]. *)
and check_direction current a b direction options lst = 
  if direction = Forwards then next_forwards current a b options lst
  else if a.number < current then next_tile' current direction b options 
      (a::lst)
  else next_tile' current direction b options lst

(** [check_forwards current a b options lst] is the next possible tiles in
    the [Forwards] direction. *)
and next_forwards current a b options lst = 
  if current = 15 && a.number =1 then next_tile' current Forwards b 
      options (a::lst)
  else if a.number > current && (a.number <> 15 || 
                                 (a.number = 15 && current = 14)) 
  then next_tile' current Forwards b options (a::lst) 
  else next_tile' current Forwards b options lst

(** [print_int_lst lst] prints the elements of [lst]. *)
let rec print_int_lst lst =
  match lst with 
  | [] -> ()
  | a::b -> print_endline (string_of_int a.number); print_int_lst b

(** [choose_tile options] is the number of the tile the user picks. *)
let rec choose_tile options =
  print_endline "Enter the number of the tile you'd like to move to: ";
  let a = read_line() in 
  match int_of_string a with 
  | num-> if List.mem num options then num
    else (print_endline "Sorry, that isn't a valid move. Choose again!"; 
          choose_tile options)
  |exception (Failure num)-> (print_endline "Sorry, that isn't a valid move. 
                                            Choose again!"; 
                              choose_tile options)

let rec next_tile player direction board = 
  let current_tile = find_tile player.tile board in 
  let options = current_tile.next in 
  let current = current_tile.number in
  let next_ones = next_tile' current direction board options [] in
  match next_ones with 
  | [] -> failwith ("Error")
  | t::[] ->  print_endline 
                (String.concat "" 
                   [player.name; " -> Tile #"; string_of_int t.number]);
    t
  | _ -> 
    print_endline "These are the tiles you can move to: ";
    print_int_lst next_ones;
    let til_num = choose_tile (find_tile player.tile board).next in 
    try let t = find_tile til_num next_ones in
      print_endline 
        (String.concat "" 
           [player.name; " -> Tile #"; string_of_int t.number]); t
    with Invalid_Tile -> 
      print_endline "That's not a valid move!"; next_tile player direction board

(*****************************************************************************)
(** The following deal with the functionality of the players. *)

(** [construct_players_r old_players new_players] constructs a [new_players] 
    list of player objects for each player name in [old_players]. *)
let rec construct_players_r old_players new_players =
  match old_players with 
  | [] -> new_players
  | a::b -> let player = 
              {name = a; 
               id= List.length new_players + 1; 
               tile = 1; 
               in_move_queue=true; 
               inventory= [(Teleporter); (Bomb); (Deed (get_random_deed ()))];
               coins=10;
               star_count = 0;
              } in 
    construct_players_r (b) (player::new_players)

let construct_players names = List.rev (construct_players_r names [])

let rec location players player_name board = 
  match players with 
  | [] -> raise Invalid_Player
  | a::b -> if a.name = player_name then 
      (find_tile a.tile board) else location b player_name board 

let player_of_name players name = List.find_opt (fun p -> p.name = name) players

let in_queue player = player.in_move_queue

let dequeue players player_name = 
  List.map (fun p -> if p.name = player_name 
             then if in_queue p then 
                 {p with
                  in_move_queue=false;
                 } 
               else raise (Failure "Tried to dequeue unqueued player")
             else p) players

let queue players player_name = 
  List.map (fun p -> if p.name = player_name
             then if not p.in_move_queue then 
                 {p with
                  in_move_queue=true;
                 } 
               else raise (Failure "Tried to queue queued player")
             else p) players

let move players board player_name dir = List.map 
    (fun p -> if p.name = player_name then 
        { p with
          tile = (next_tile p dir board).number; 
        } else p) players

let next_in_queue players = List.find_opt (fun p -> in_queue p) players

(*****************************************************************************)
(** The following are getters for the different fields of the board, tiles, and 
    players. *)

let get_game tile = tile.game

(*****************************************************************************)
(* The following deal with the store transaction. *)

(** print_store is a list [possibilities] of all of the items [player] can 
    purchase at this store based on their prices. 
    It also prints the contents of [store]. *)
let rec print_store store possibilities player = 
  match store with 
  | [] -> possibilities
  | (i,p)::b -> print_endline (string_of_item i ^ ": " ^ string_of_int p 
                               ^ " coins"); 
    if p <= player.coins then print_store b ((i,p)::possibilities) player 
    else print_store b possibilities player

(** [price_and_star_transaction item player] is [player] with the coins
    and star field updated after the [item] transaction*)
let item_transaction item player = 
  match item with 
  | (i,p) -> {
      name = player.name;
      id = player.id;
      tile = player.tile;
      in_move_queue = player.in_move_queue;
      inventory = i::player.inventory;
      coins = player.coins - p;
      star_count = player.star_count;
    }

(** [make_star_transaction p] is the updated [p] after they buy a star. *)
let make_star_transaction p = 
  {
    p with
    tile = 1;
    coins = p.coins - 10;
    star_count = p.star_count + 1;
  }

let make_star_transaction_no p =
  {
    p with 
    tile = 1
  }
(** [process_answer player answer] is [player] if player does not want to 
    visit the store and [make_star_transaction] otherwise. *)
let rec process_answer player answer = 
  match answer with 
  | "yes" -> make_star_transaction player
  | "no" -> print_endline "Thank you for visiting the star store. Goodbye!";
    make_star_transaction_no player 
  | _ -> print_endline "Sorry, that command isn't understood. Type 'yes' if you
  would like to purchase a star and 'no' otherwise."; 
    let answer =
      read_line () in process_answer player answer

(** [star_transaction tile player] is the [player] after conducting a 
    star transaction at a star store on tile [tile], if there is one.*)
let star_transaction tile player = 
  match tile.store with 
  | Star_Store -> if player.coins < 10 then 
      (print_endline "Sorry, you don't have enough coins to buy a star now. \
      Stars are 10 coins. Try again later!";
       make_star_transaction_no player)
    else   let str = "You can purchase a star! The price of a star is 10 coins. 
  You have " ^ string_of_int player.coins ^ " coins. Type 'yes' if you would 
  like to purchase a star and 'no' otherwise."; in print_endline str; let 
        answer = read_line () in process_answer player answer
  | _ -> player

(** [print_init tile player] is the initial command for [store_transaction]. *)
let print_init tile player = 
  ANSITerminal.(print_string [white]
                  "~~~~~~~~~~~~~~~~\n");
  ANSITerminal.(print_string [white]
                  "You landed on a store tile!\n");
  print_endline ("You have " ^ string_of_int player.coins ^ " coins to spend.");
  print_endline ""

(** [store_transaction tile player] is the [player] after conducting a 
    transaction at the store on tile [tile], if there is one.*)
let rec store_transaction tile player =
  match tile.store with 
  | Store p -> print_init tile player;
    ANSITerminal.(print_string [blue]) "Here are the items in the store:\n"; 
    let possible_items = print_store p [] player in 
    if (List.length possible_items = 0) then 
      (print_endline "Sorry, you don't have enough coins 
      to purchase an item right now. Try again later!"; player) else
      has_items tile player possible_items
  | Star_Store -> ANSITerminal.(print_string [white]
                                  "~~~~~~~~~~~~~~~~\n");
    ANSITerminal.(print_string [blue]
                    "Congratulations! You have reached the star store tile!\n");
    print_endline ("You have " ^ string_of_int player.coins ^ " coins to spend.\
    \n");
    print_endline ""; star_transaction tile player
  | _ -> player

(** [has_items tile player possible_items] is a helper for [store_transaction]
    when the player has enough coins to purchase an item. *)
and has_items tile player possible_items = 
  (ANSITerminal.(
      print_string [white] 
        "Enter the name of the item you'd like to purchase. 
    Or, enter 'leave' to leave the store.\n");
   ANSITerminal.(print_string [white]
                   "~~~~~~~~~~~~~~~~\n");
   let item = read_line () in 
   if item = "leave" then player else begin 
     match List.find_opt (fun i -> 
         String.uppercase_ascii 
           (string_of_item (fst i)) = 
         (String.uppercase_ascii item)) possible_items with 
     | None -> print_endline "Sorry, that's not a valid item. Try again!"; 
       store_transaction tile player
     | Some i -> 
       ANSITerminal.(print_string [green] "Thank you for purchasing an item! 
       It has been added to your inventory.\n");
       print_endline "";
       item_transaction i player end)

let do_property_tax player tile players = 
  match List.find_opt 
          (fun s -> match s with | Property _ -> true | _ -> false) 
          tile.structures with 
  | None ->  players
  | Some Property (p,o) -> 
    print_endline 
      (String.concat "" ["You landed on "; o; "'s "; p; "! \
      You must pay a property tax (if you have any coins)!"]); 
    transfer_coins player.name o players 5 
  | _ -> failwith "???" 

let do_store_interaction player tile players = 
  List.map (fun p -> if p = player then store_transaction tile p else p) players 

(** [winning_condition player] is whether [player] has won. *)
let winning_condition player = if player.star_count >= 2 then true else false 

let rec winner player players = match players with 
  | [] -> failwith "Not found"
  | a::b -> if a.name = player.name 
    then winning_condition a else winner player b

(** [collect_coins tile player] collects the coins for [player]. *)
let collect_coins tile player = 
  ANSITerminal.(
    print_string [green] 
      ("You earned " ^ string_of_int tile.tile_coins ^ " coins!\n"));

  let coins = tile.tile_coins in 
  {
    player with
    coins = player.coins + coins;
  }

let do_coin_collection player tile players = 
  List.map (fun p -> if p = player then collect_coins tile p else p ) players 

(* Defines how the board should be printed *)
let bottom_row = [1;2;4;6;8;9;12;14;15]
let middle_row = [3;5;7;11;13]
let top_row = [10]

(* TODO: generate these connections from only the board 
   and the above row expressions. *)
let up_connections = [(2,3); (8,11); (7,10)]
let down_connections = [(10,13); (13,14)]
let right_connections = [(1,2); (2,4); (4,6); (6,8); (8,9); (9,12); (12,14); 
                         (14,15); (3,5); (5,7); (7,11); (11,13)]

let row_string_builder arr = ""

type dir = Up | Down | Right

type cell = Empty | Connection of dir | Tile of tile | StarBot | StarTop

(** [string_of_cell_top c players] is the top row of the string-form
    of the cell [c] printed on a board with players [players].*)
let string_of_cell_top c players = match c with 
  | Empty ->            "        "
  | Connection Up ->    "     ^  "
  | Connection Down ->  "   \\    "
  | Connection Right -> "        "
  | Tile t -> 
    let n = (if t.number < 10 then string_of_int t.number ^ " " 
             else string_of_int t.number) in
    " / "^n^" \\ "
  | StarTop -> "        " 
  | StarBot -> " __/ \\__"

(**  [string_of_cell_mid c players] is the middle row of the string-form
     of the cell [c] printed on a board with players [players].*)
let string_of_cell_mid c players = match c with 
  | Empty ->            "        "
  | Connection Up ->    "    /   "
  | Connection Down ->  "    \\   "
  | Connection Right -> "  ———>  "
  | Tile t -> 
    let players_on_t = players_on_tile players t in
    let players_string = String.concat "" 
        (["P"] @ (List.map (fun p -> string_of_int p.id) players_on_t)) in
    let p = match String.length players_string with 
      | 1 -> "     "
      | 2 -> players_string^"   "
      | 3 -> players_string^"  "
      | 4 -> players_string^" "
      | 5 -> players_string
      | _ -> failwith "Invalid players." in
    "| "^p^"|"
  | StarTop -> "        "
  | StarBot -> " '.   .'"

(** [string_of_structs t] is the string representation of the bottom 
    row of a tile [t], which may contain special characters that indicate
    that special structures have been built on the tile.*)
let string_of_structs t = 
  let things = 
    (match t.store with 
     | Empty_Store -> " "
     | Store _  -> "$"
     | Star_Store -> "S") ^ 
    (let structs_string = 
       (String.concat "" 
          (List.map (fun s -> match s with 
               | PlantedBomb -> "¤"
               | Property _ -> "©") t.structures)) in 
     match String.length structs_string with
     | 0 -> "  "
     | 1 -> " " ^ structs_string
     | 2 -> " " ^ structs_string
     | 3 -> "" ^ structs_string
     | 4 -> "" ^ structs_string
     | _ -> failwith "impossible") ^
    (match t.game with 
     | Empty -> " "
     | Game _ -> "G") in 
  " \\"^things^"/ "

let string_of_cell_bot c players = match c with 
  | Empty ->            "        "
  | Connection Up ->    "   /    "
  | Connection Down ->  "     v  "
  | Connection Right -> "        "
  | Tile t -> string_of_structs t    
  | StarTop -> "    ^   "
  | StarBot -> " /.' '.\\"
(*"["^(string_of_int t.number)^"]"*)

(** [string_of_arr arr players] is the string which represents a list of [cell]s
    [arr] on the board, with players [players]*)
let rec string_of_arr arr players = 
  (List.fold_left (fun a b -> a ^ string_of_cell_top b players) "" arr)
  ^ "\n" ^ (List.fold_left (fun a b -> a ^ string_of_cell_mid b players) "" arr)
  ^ "\n" ^ (List.fold_left (fun a b -> a ^ string_of_cell_bot b players) "" arr)

(** [string_of_board_arrs arrs players ""] is the string representing the 
    entire board represented by lists of [cell]s [arrs], with players [players].*)
let rec string_of_board_arrs arrs players acc = match arrs with 
  | [] -> acc
  | a::r -> string_of_board_arrs r players acc ^ "\n" ^ 
            (string_of_arr a players)

(** [index e lst 0] is the index at which [e] first appears in list [lst]*)
let rec index e lst acc = 
  match lst with 
  | [] -> raise (Failure "Not in list")
  | h::t -> if e = h then acc else index e t (1+acc)

(** [bot_row_array_builder tiles arr] is the list of [cell]s representing the
    bottom row of the board in printable-form.*)
let rec bot_row_array_builder tiles (acc: tile list) (arr: cell list) = 
  if List.length acc = 0 then let start_tile = find_tile 1 tiles 
    in bot_row_array_builder tiles 
      [start_tile] [Tile start_tile; Connection Right] else
    let last_tile = List.hd acc in 
    match last_tile.number with 
    | 15 -> (Connection Right)::arr
    | _ -> begin
        match (List.find_opt (fun t -> 
            List.mem t.number bottom_row 
            && List.mem 
              t.number last_tile.next && t.number > last_tile.number) 
            tiles
          ) with 
        | None -> raise Invalid_Board
        | Some t -> bot_row_array_builder tiles (t::acc) 
                      ((Tile t)::(Connection Right)::arr)
      end

(** 
   [upper_row_array_builder tiles this_row previous_row previous_row_arr []] 
   is the list of [cell]s representing an upper row of the board in 
   printable-form. The row being constructed is given by the list of
   tile id's [this_row], and the structure and layout of the row is 
   determined from [previous_row] and [previous_row_arr].*)
let upper_row_array_builder (tiles: tile list) (this_row: int list) 
    (previous_row: int list) (previous_row_arr: cell list) (acc: tile list) 
    (arr: cell list) = 

  let rec builder acc arr = 
    let last_tile_nexts = try 
        List.filter (fun t -> 
            List.mem ((List.hd acc).number,t) right_connections)
          (List.hd acc).next 
      with (Failure _) -> [] in 
    match (List.find_opt 
             (fun t -> List.mem t.number this_row && List.mem t.number 
                         last_tile_nexts) tiles) with 
    | None -> begin match 
          (List.find_opt (fun c -> match c with 
               | Tile t -> begin match List.find_opt (
                   fun ctn -> 
                     (fst ctn) = 
                     t.number && List.mem (snd ctn) this_row) up_connections 
                   with 
                   | None -> false 
                   | Some t' -> 
                     List.filter 
                       (fun ctn -> snd ctn = (snd t')) right_connections = [] 
                     && (not (List.mem (find_tile (snd t') tiles) acc)) end
               | _ -> false)) previous_row_arr 
        with 
        | None -> arr
        | Some Tile t -> 
          let diff = 
            (index (Tile t) previous_row_arr 0)-(List.length arr) + 1 
          in 

          let next_tile = find_tile 
              (snd (List.find (fun ctn -> fst ctn = t.number) up_connections)) 
              tiles in
          let new_arr = (Tile next_tile)::(List.init (diff+1) (fun n -> Empty)) 
                        @ arr in 
          builder (next_tile::acc) new_arr
        | _ -> raise Invalid_Board
      end
    | Some t -> builder (t::acc) ((Tile t)::(Connection Right)::arr) 
  in builder acc arr 

(** 
   [connection_row_array_builder tiles below_row_arr above_row_arr []]
    is the list of [cell]s representing an row of vertical connections of
    the board in printable-form. The row being constructed is given by the list 
    of tiles [tiles], and the structure and layout of the row is 
    determined from [below_row_arr] and [above_row_arr].*)
let connection_row_array_builder (tiles: tile list) (below_row_arr : cell list) 
    (above_row_arr: cell list) (arr: cell list) = 
  let rec builder arr = 
    let current_pos = List.length arr in 
    if current_pos = 0 then (builder ((Empty)::arr)) else
      try try 

          let exists_connection arr connectns = 
            match List.nth arr (current_pos-1) with 
            | Tile t -> begin 
                match (List.find_opt (fun ctn -> fst ctn = t.number) connectns) 
                with 
                | None -> false
                | Some t -> true 
              end
            | _ -> false in 

          match exists_connection below_row_arr up_connections, 
                exists_connection above_row_arr down_connections with 
          | true, false -> builder ((Connection Up)::arr)
          | false, true -> builder ((Connection Down)::arr)
          | false, false -> builder ((Empty)::arr)
          | true, true -> raise Invalid_Board
        with Invalid_argument _ -> raise Invalid_Board
      with Failure _ -> arr
  in 
  builder arr

(**[board_arrs players tiles] is the [cell list list] which represents
   the current game board.*)
let board_arrs players tiles = 
  let bot_row_arr = List.rev (bot_row_array_builder tiles [] []) in 
  let mid_row_arr = 
    match List.rev 
            (upper_row_array_builder 
               tiles middle_row bottom_row bot_row_arr [] []) with 
    | [] -> failwith "impossible" 
    | h::t -> StarTop::t in 
  let top_row_arr = List.rev 
      (upper_row_array_builder tiles top_row middle_row mid_row_arr [] []) in 
  let top_connection_arr = List.rev 
      (connection_row_array_builder tiles mid_row_arr top_row_arr []) in
  let bot_connection_arr = 
    match List.rev 
            (connection_row_array_builder tiles bot_row_arr mid_row_arr []) with 
    | [] -> failwith "impossible"
    | h::t -> StarBot::t in
  [bot_row_arr; 
   bot_connection_arr;
   mid_row_arr; 
   top_connection_arr;
   top_row_arr]

(** [board_string_builder players board] is the [string] representing the board
    [board] with players [players]*)
let board_string_builder players board = 
  string_of_board_arrs (board_arrs players board) players ""

(** [players_string players] is the [string] representing the players in
    the game.*)
let players_string players = List.fold_left 
    (fun a b -> a ^ "\n" ^ ("P" ^ string_of_int b.id ^ ":" ^ b.name)) "" players

(** [print_board players board] prints a [string] representation of the 
    game board being played by [players].*)
let print_board players board = 
  print_endline (board_string_builder players board
                 ^ "\n G: Minigame Tile   $: Store Tile  ¤: Bomb  ©: Property\n"
                 ^ "\n" ^ (players_string players) ^ "\n"
                )
