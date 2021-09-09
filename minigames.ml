open Random

(** The different types of mini games. *)
type minigame = Typing | Multiplication | Scramble | Trivia | RPS

(** The result of the mini game. *)
type game_result = int

let _ = Random.self_init()

(**[start_game u] is true if the user enters 'start'. *)
let rec start_game u = 
  print_endline "Type 'start' when you are ready to begin.";
  print_string  "> ";
  match read_line() with 
  |"start" -> true
  |_ -> start_game ()

(** [random_element arr] is a random element of [arr]. *)
let random_element arr =
  let n = Random.int (Array.length arr) in
  Array.get arr n;;

(** [end_timer] is true if the user completes the task within [max_time] 
    seconds *)
let end_timer start_time max_time = 
  let stop_time = Unix.gettimeofday () in 
  let difference = stop_time -. start_time in 
  if difference < max_time then true else false

(** [end_game_print start_time max_time] is 1 if the mini game was won
    and 0 if it was lost. *)
let end_game_print start_time max_time = 
  let result = end_timer start_time max_time  in 
  match result with
  |true -> ANSITerminal.(
      print_string [blue]
        "\nYou won! You get to roll again, and you earned 10 coins!\n"); 
    print_endline ""; 1
  |false -> ANSITerminal.(
      print_string [red]
        "\nYou didn't beat the timer. Try again next time!\n"); 
    print_endline ""; 0

(** [quit_early x] is called if a user fails a mini game before the timer
    runs out. *)
let quit_early x = 
  ANSITerminal.(print_string [red]
                  "You lost the game. Try again next time!"); 
  print_endline ""; 0

let play_typing x : game_result =
  print_endline "We will now play the typing game. You must type the words as 
  they appear and press enter as quickly as you can.";
  let start = start_game () in 
  if start then
    let start_time = Unix.gettimeofday () in
    let rec game_1' acc = 
      if acc = 0 then end_game_print start_time 20.0 else
        let word = random_element [|"martha"; "corona"; "zoom"; "hi";
                                    "online"; "coding"|] in
        print_endline (word);
        print_string  "> ";
        let c = read_line () in 
        match c with
        | input -> if word = input then game_1' (acc - 1) else quit_early 1
    in game_1' 5
  else failwith "game not started"

let play_multiplication x : game_result =
  print_endline "We will now play the multiplication game. You must do the math
    and press enter as quickly as you can.";
  let start = start_game () in 
  if start then
    let start_time = Unix.gettimeofday () in
    let rec game_2' acc = 
      if acc = 0 then end_game_print start_time 15.0 else
        let fst = Random.int 10 in 
        let snd = Random.int 10 in 
        print_endline ((string_of_int fst) ^ " * " ^ (string_of_int snd));
        print_string  "> ";
        let c = read_line () in 
        match c with
        | num -> try 
            if int_of_string num = (fst * snd) then game_2' (acc - 1) 
            else quit_early 1 
          with _ -> quit_early 1
    in game_2' 5
  else failwith "game not started"

let play_scramble x : game_result =
  print_endline "We will now play unscramble! You will be given a scrambled 
  word and must unscramble it before the time runs out. Hint: all of the
  words are related to Cornell!";
  let start = start_game () in 
  if start then
    let start_time = Unix.gettimeofday () in
    let rec game_3' acc = 
      if acc = 0 then end_game_print start_time 60.0 else
        let words = [|("uris", "sriu"); ("pollack", "alpkolck"); 
                      ("statler", "tsratle"); ("gates", "stega")|] in
        let w = random_element words in
        print_endline (snd w);
        print_string  "> ";
        let c = read_line () in 
        match c with
        | word -> if word = fst w then game_3' (acc - 1) else quit_early 1
    in game_3' 3
  else failwith "game not started"

let play_trivia x : game_result =
  print_endline "We will now play the trivia game! You will be given a question 
  about Cornell and must answer it before the time runs out.";
  let start = start_game () in 
  if start then
    let start_time = Unix.gettimeofday () in
    let rec game_4' qs acc = 
      if acc = 0 then end_game_print start_time 60.0 else 
        let pos = Random.int (List.length qs) in 
        let q = List.nth qs pos in
        print_endline (fst q);
        print_string  "> ";
        let c = read_line () in 
        match c with
        | a -> if a = snd q then 
            game_4' (List.filter (fun (x,y) -> y <> snd q) qs) (acc - 1) 
          else quit_early 1
    in game_4' 
      [("What's the first name of Cornell's founder?", "ezra"); 
       ("How many steps does McGraw Tower have?", "161"); 
       ("What year was Cornell founded?", "1865"); 
       ("Fill in the blank: Robert C. Baker was an inventor and \
         Cornell professor who invented the ---- as well as many other 
                  poultry-related inventions.", "chicken nugget")]  3
  else failwith "game not started"

(** [winner p1_ans p2_ans] is true if [p1_ans] beats [p2_ans] in
    the round of rock, paper, scissors. *)
let rec winner p1_ans p2_ans = 
  match p1_ans, p2_ans with
  |"paper", "rock" -> true
  |"scissors", "paper" -> true
  |"rock", "scissors" -> true
  |_ -> false

(** [end_rps turns_left p1_score p2_score p1 p2] is 2 if player 1 wins,
    3 if player 2 wins, and 4 if there is a draw. *)
let end_rps turns_left p1_score p2_score p1 p2  = 
  match p1_score with 
  |3 -> ANSITerminal.(
      print_string [green]
        ("\nCongrats " ^ p1 ^ ", you won! You earned 10 coins.\n")); 
    print_endline ""; 2
  |_ -> match p2_score with 
    |3 -> ANSITerminal.(
        print_string [green]
          ("\nCongrats " ^ p2 ^ ", you won! You earned 10 coins.\n")); 
      print_endline ""; 3
    |_ -> ANSITerminal.(print_string [green]
                          "\nIt was a tie game."); print_endline ""; 4


(** [answer u] is the player's choice of rock, paper, or scissors. *)
let answer u = 
  match read_line() with
  |"r" -> "rock"
  |"p" -> "paper"
  |"s" -> "scissors"
  |_ -> "not valid" 

(** [print_score p1 p2 p1_score p2_score] prints the current score of the rock,
    paper, scissors game between [p1] and [p2]. *)
let rec print_score p1 p2 p1_score p2_score =  
  print_endline ""; print_endline "The score is currently:";
  print_endline ( p1 ^ ": " ^ string_of_int (p1_score) ^ " " ^ 
                  p2 ^ ": " ^ string_of_int (p2_score))

(** [print_choices p1 p2 p1_ans p2_ans] prints the choices [p1] 
    and [p2] in the current round of rock, paper, scissors. *)
let print_choices p1 p2 p1_ans p2_ans = 
  ANSITerminal.(print_string [blue] "\n~~~~~~~~~~~~~~\n");
  ANSITerminal.(print_string [blue] (p1 ^ ", you chose: " ^ p1_ans ^ "\n"));
  ANSITerminal.(print_string [blue] (p2 ^ ", you chose: " ^ p2_ans ^ "\n"));
  print_endline ""

let play_rps (p1:string) (p2:string) : game_result =
  print_endline "We will now play rock paper scissors. When it's your turn, type 
'r' for rock, 'p' for paper, or 's' for scissors, and then press enter. Please
be careful you are pressing the correct key! The first to win 3 of 5 matches
will win the game."; print_endline "";
  let start = start_game () in  
  if start then print_string  "> ";
  let rec game turns_left p1_score p2_score = 
    if turns_left < 1 || p2_score = 3 || p1_score = 3 then 
      end_rps turns_left p1_score p2_score p1 p2 else
      let p1_ans = 
        print_score p1 p2 p1_score p2_score;
        print_endline ""; print_endline (p1 ^ ", enter your move.");
        print_string "\x1b[8m"; answer () 
      in print_endline "\x1b[0m"; print_endline (p2 ^ ", enter your move.");
      let p2_ans = print_string "\x1b[8m"; answer () in print_string "\x1b[0m";
      print_choices p1 p2 p1_ans p2_ans;
      let outcome = match winner p1_ans p2_ans with 
        |true -> "p1"
        |false -> if p1_ans = p2_ans then "draw" else "p2" in 
      match outcome with 
      |"p1" -> ANSITerminal.(print_string [blue]
                               (p1 ^ " won that round. \n~~~~~~~~~~~~~~")); 
        game (turns_left - 1) (p1_score + 1) (p2_score) 
      |"p2" -> ANSITerminal.(print_string [blue]
                               (p2 ^ " won that round. \n~~~~~~~~~~~~~~"));
        game (turns_left - 1) (p1_score) (p2_score + 1)
      |"draw" -> ANSITerminal.(print_string [blue]
                                 ("There was a draw. \n~~~~~~~~~~~~~~"));
        game (turns_left - 1) (p1_score) (p2_score)
      |_ -> failwith "???" 
  in game 5 0 0
