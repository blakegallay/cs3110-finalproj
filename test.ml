open OUnit2
open Board
open Move
open Minigames

(* We used OUnit to test all of the manual functions in the board module. 
   These functions deal with manipulating players and tiles. We manually create 
   boards and players and test things like finding the next tile, moving a player, 
   dequeing a player, etc. Much of our game deals with user input or random 
   placement of things like minigames and stores, so these we tested by playing 
   the game in the terminal. 
   Test cases were developped using glass box testing. We tested our functions
   with knowledge of the data structures and implementations. We used glass box
   to ensure that our tests were extensive and that all parts of our functions
   were correct.
   Our combination of OUnit and manually testing helped us confirm that our
   system was correct. These functions in board form much of the basis for player
   movement and interaction with the board. By using OUnit to test these and 
   demonstrate that all parts of the board and player structures were correct, 
   we were able to use manual testing to focus on errors with user input and 
   with the states of the game. It was much easier to locate and isolate
   errors. *)

let tile1 = {
  number = 1;
  game = Empty;
  next = [2];
  structures = [];
  store = Empty_Store;
  tile_coins = 2
}
let tile2 = {
  number = 2;
  game = Empty;
  next = [3];
  structures = [];
  store = Empty_Store;
  tile_coins = 3
}

let tile3 = {
  number = 3;
  game = Empty;
  next = [1];
  structures = [];
  store = Empty_Store;
  tile_coins = 3
}

let tile6 = {
  number = 4;
  game = Game (Typing);
  next = [2];
  structures = [];
  store = Empty_Store;
  tile_coins = 3
}

let tile4 = {
  number = 3;
  game = Empty;
  next = [1];
  structures = [PlantedBomb];
  store = Empty_Store;
  tile_coins = 3
}

let tile5 = {
  number = 3;
  game = Empty;
  next = [1];
  structures = [PlantedBomb; Property ("hi","b")];
  store = Empty_Store;
  tile_coins = 3
}
let tile7 = {
  number = 4;
  game = Empty;
  next = [1];
  structures = [];
  store = Star_Store;
  tile_coins = 0;
}
let tile8 = {
  number = 15;
  game = Empty;
  next = [1];
  structures = [];
  store = Star_Store;
  tile_coins = 0
}
let tile9 = {
  number = 2;
  game = Empty;
  next = [1];
  structures = [];
  store = Star_Store;
  tile_coins = -5
}
(*Player A*)
let player1 = {
  name = "a";
  id = 1;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 10;
  star_count = 0
}
let player6 = {
  name = "a";
  id = 1;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 10;
  star_count = 2
}
let player7 = {
  name = "a";
  id = 1;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 13;
  star_count = 0
}
let player8 = {
  name = "a";
  id = 1;
  tile = 2;
  in_move_queue = true;
  inventory = [];
  coins = 10;
  star_count = 0
}
let player9 = {
  name = "a";
  id = 1;
  tile = 2;
  in_move_queue = true;
  inventory = [];
  coins = 5;
  star_count = 0
}
let player14 = {
  name = "a";
  id = 1;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 5;
  star_count = 0
}
let player13 = {
  name = "a";
  id = 1;
  tile = 4;
  in_move_queue = true;
  inventory = [];
  coins = 5;
  star_count = 0 
}
(*Player B*)
let player2 = {
  name = "b";
  id = 2;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 10;
  star_count = 0
}

let player4 = {
  name = "b";
  id = 2;
  tile = 1;
  in_move_queue = false;
  inventory = [];
  coins = 10;
  star_count = 0
}

let player5 = {
  name = "b";
  id = 2;
  tile = 2;
  in_move_queue = true;
  inventory = [];
  coins = 10;
  star_count = 0
}
let player11 = {
  name = "b";
  id = 2;
  tile = 1;
  in_move_queue = true;
  inventory = [];
  coins = 15;
  star_count = 0
}

(* Player C*)
let player3 = {
  name = "c";
  id = 2;
  tile = 1;
  in_move_queue = false;
  inventory = [];
  coins = 10;
  star_count = 0
}
let player10 = {
  name = "c";
  id = 2;
  tile = 15;
  in_move_queue = false;
  inventory = [];
  coins = 10;
  star_count = 0
}
let player12 = {
  name = "c";
  id = 2;
  tile = 15;
  in_move_queue = false;
  inventory = [];
  coins = 10;
  star_count = 2
}

let board1 = [tile1;tile2;tile3]

let tests = [
  "test location of player1 as the only player and only one tile" >:: 
  (fun _ -> assert_equal (tile1) (location [player1] "a" [tile1]));
  "test location of player1 as the only player with multiple tiles" >:: 
  (fun _ -> assert_equal (tile1) (location [player1] "a" board1));
  "test location of player3 with multiple players" >:: 
  (fun _ -> assert_equal (tile1) (location [player1;player2; player3] "c" 
                                    [tile1]));
  "test next tile for a player on tile1" >:: 
  (fun _ -> assert_equal (tile2) (next_tile player1 Forwards board1));
  "test next tile for a player on last tile" >::
  (fun _ -> assert_equal (tile1) (next_tile player10 Forwards [tile1;tile8]));
  "test dequeue for a game with one player" >:: 
  (fun _ -> assert_equal ([player4]) (dequeue [player2] "b"));
  "test dequeue for a game with multiple players" >:: 
  (fun _ -> assert_equal ([player1;player4;player3]) 
      (dequeue [player1;player2;player3] "b"));
  "test queue for a game with one player" >:: 
  (fun _ -> assert_equal ([player2]) (queue [player4] "b"));
  "test queue for a game with multiple players" >:: 
  (fun _ -> assert_equal ([player1;player2;player3]) 
      (queue [player1;player4;player3] "b"));
  "test move for a game with one player" >::
  (fun _ -> assert_equal ([player5]) (move [player2] board1 "b" Forwards));
  "test move for a game with multiple players" >:: 
  (fun _ -> assert_equal ([player1;player5;player3]) 
      (move [player1;player2;player3] board1 "b" Forwards));
  "test move for tile at the end of the board" >::
  (fun _ -> assert_equal ([player3]) (move [player10] [tile1;tile2;tile8] 
                                        "c" Forwards));
  "test next in queue for some and a game with one player" >::
  (fun _ -> assert_equal (Some player1) (next_in_queue [player1]));
  "test next in queue for some and a game with multiple players" >:: 
  (fun _ -> assert_equal (Some player1) (next_in_queue 
                                           [player3;player1;player2]));
  "test next in queue for none and a game with one player" >:: 
  (fun _ -> assert_equal (None) (next_in_queue [player3]));
  "test next in queue for none and a game with multiple players" >::
  (fun _ -> assert_equal (None) (next_in_queue [player3;player4]));
  "test get game for empty" >:: (fun _ -> assert_equal (Empty) 
                                    (get_game tile1));
  "test get game for non empty" >:: 
  (fun _ -> assert_equal (Game (Typing)) (get_game tile6));
  "test winning for true and one player" >:: (fun _ -> assert_equal (true) 
                                                 (winner player6 [player6]));
  "test winning for false and one player" >:: (fun _ -> assert_equal (false) 
                                                  (winner player1 [player1]));
  "test winner for true and multiple winners" >:: 
  (fun _ -> assert_equal (true) (winner player6 [player6;player12]));
  "test winner for false but other winners" >::
  (fun _ -> assert_equal (false) (winner player1 [player12;player1]));
  "test players on tile for one player" >:: 
  (fun _ -> assert_equal ([player1]) (players_on_tile [player5;player1] tile1));
  "test players on tile for multiple players" >:: 
  (fun _ -> assert_equal ([player1;player2;player3]) 
      (players_on_tile [player1;player2;player3] tile1));
  "test in queue for false" >:: 
  (fun _ -> assert_equal (false) (Board.in_queue player3));
  "test in queue for true" >:: 
  (fun _ -> assert_equal (true) (Board.in_queue player1));
  "test string of item for teleporter" >::
  (fun _ -> assert_equal ("Teleporter") (Board.string_of_item Teleporter));
  "test string of item for deed" >:: 
  (fun _ -> assert_equal ("Deed to the abc") (Board.string_of_item 
                                                (Deed("abc"))));
  "test string of item for bomb" >::
  (fun _ -> assert_equal ("Bomb") (string_of_item Bomb));
  "test string of structure for planted bomb" >::
  (fun _ -> assert_equal ("Land Mine") (Board.string_of_structure PlantedBomb));
  "test string of structure for property" >::(fun _ -> assert_equal 
                                                 ("a's tile") 
                                                 (Board.string_of_structure 
                                                    (Property ("tile","a"))));
  "test remove structure for board w/ one tile" >:: 
  (fun _ -> assert_equal ([tile3]) (remove_structure PlantedBomb tile4 
                                      [tile4]));
  "test remove structure for board w/ multiple tiles" >::
  (fun _ -> assert_equal ([tile2;tile3]) 
      (remove_structure PlantedBomb tile4 [tile2;tile4]));
  "test remove structure for board w/ multiple tiles and tile w/
   multiple structures" >::
  (fun _ -> assert_equal ([tile2;tile4]) 
      (remove_structure (Property ("hi","b")) tile5 [tile2;tile5]));
  "test find player for one player" >:: 
  (fun _ -> assert_equal (player1) (find_player "a" [player1]));
  "test find player for multiple players" >:: 
  (fun _ -> assert_equal (player1) (find_player "a" [player4;player1]));
  "test find tile for board with one tile" >:: 
  (fun _ -> assert_equal (tile1) (find_tile 1 [tile1]));
  "test find tile for board with multiple tiles" >::
  (fun _ -> assert_equal (tile1) (find_tile 1 [tile2;tile1]));
  "test players on tile for one player" >::
  (fun _ -> assert_equal [player1] (players_on_tile [player1] tile1));
  "test players on tile for multiple players" >::
  (fun _ -> assert_equal [player1;player2] 
      (players_on_tile [player1;player5;player2] tile1));
  "test coin collection for one player game" >::
  (fun _ -> assert_equal ([player7]) (do_coin_collection player1 tile5 
                                        [player1]));
  "test coin collection for multi-player game" >::
  (fun _ -> assert_equal ([player7;player2]) 
      (do_coin_collection player1 tile5 [player1;player2]));
  "test coin collection for 0 coins" >:: 
  (fun _ -> assert_equal ([player8]) (do_coin_collection player8 tile7 
                                        [player8]));
  "test coin collection for decreasing player's coins" >::
  (fun _ -> assert_equal ([player9]) (do_coin_collection player8 tile9 
                                        [player8]));
  "test star transaction for not enough coins" >::
  (fun _ -> assert_equal ([player14]) (do_store_interaction player9 tile7 
                                         [player9]));
  "test star transaction for not enough coins and multiple players" >::
  (fun _ -> assert_equal ([player2;player14]) 
      (do_store_interaction player9 tile7 [player2;player9]));
  "test store transaction for no store" >::
  (fun _ -> assert_equal ([player9]) (do_store_interaction player9 tile1 
                                        [player9]));
  "test do property tax for no structure" >::
  (fun _ -> assert_equal ([player1]) (do_property_tax player1 tile1 [player1]));
  "test do property tax for structure" >::
  (fun _ -> assert_equal ([player9;player11]) 
      (do_property_tax player8 tile5 [player8;player2]));
  "test do property tax for structure and more than two players" >::
  (fun _ -> assert_equal ([player3;player9;player11]) 
      (do_property_tax player8 tile5 [player3;player8;player2]));
]

let suite =
  "test suite"  >::: List.flatten [
    tests
  ]

let _ = run_test_tt_main suite
