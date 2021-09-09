type player_data = Board.player list

type board_data = Board.board

type game_spec = {player_data: player_data; board_data: board_data}

let rec initialize_players lst cnt =
  let _ = Sys.command "clear" in
  let rec print_players acc = function 
    | [] -> ()
    | p::r -> print_endline (String.concat "" 
                               ["Player "; string_of_int acc; ": "; p; ""]); 
      print_players (acc+1) r in
  print_players 1 lst;
  print_endline "\n Enter the name of the next player you'd like to add: \n";
  print_string "> ";
  match read_line () with 
  | "stop" -> if List.length lst > 0 then lst else initialize_players lst cnt
  | a -> if List.mem a lst then 
      (print_endline "No two players can have the same name!\n"; 
       Unix.sleepf 0.25; initialize_players lst cnt)
    else
      let new_players = lst @ [a] in if List.length (new_players) >= cnt then 
        let _ = Sys.command "clear" in 
        print_players 1 (new_players); (new_players) 
      else initialize_players (new_players) cnt

let rec init_player_data x = 
  ANSITerminal.(print_string [green]
                  "This game is designed to be played by between 2 and 4 players.
Please enter a number in this range to indicate how many will be playing. \n");
  print_string "> ";
  match read_line () with 
  | a -> begin match int_of_string_opt a with 
      | None -> print_endline "\nThat's not a number! Try again.\n"; 
        init_player_data ()
      | Some n -> if n < 5 && n > 1 then 
          let players = initialize_players [] n in
          Board.construct_players players 
        else 
          let _ = print_endline 
              (String.concat "" 
                 [string_of_int n; " is not in the range 2-4! Try again!\n"])
          in init_player_data ()
    end

let rec init_board_data' lst = 
  [Minigames.Multiplication; Minigames.Typing; Minigames.Scramble;
   Minigames.Trivia; Minigames.RPS]

let init_board_data x = let games = init_board_data' [] 
  in Board.construct_board games

type phrase = Player of Board.player
            | Minigame of Minigames.minigame
            | StandardTurn | MinigameLose | MinigameWin
            | SinglePlayer | TwoPlayer
            | SmallDie | MediumDie | BigDie