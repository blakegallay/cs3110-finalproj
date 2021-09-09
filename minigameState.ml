module MinigameState : State.State = struct

  (** The type of minigames. *)
  type t = {game: Minigames.minigame; 
            num_players: int; 
            triggered_by: Board.player} 

  (** The type of states. *)
  type state_spec = {state_info: t; game_info:Game.game_spec}

  (** The type of new states. *)
  type out = 
    |Same of state_spec 
    | Different of { next_state: State.stateType; 
                     game_info: Game.game_spec; 
                     phrases: Game.phrase list
                   }

  (** Raised when a player attemps to challenge a player that doesn't exist. *)
  exception UnknownPlayer

  (** Raised when a player attemps to challenge themselves. *)
  exception InvalidChallenge

  (** [index p lst acc] is the index of player [p] in [lst]. *)
  let rec index p lst acc = 
    match lst with
    | [] -> failwith "empty"
    | h::t -> if (h = p) then acc else index p t (acc + 1)

  (** [replace_player lst i p] is [lst] but with the element at index [i]
      replaced by [p]. *)
  let rec replace_player lst i p =
    match lst with
    | [] -> lst
    | h::t -> if (i = 0) then
        p::(replace_player t (i - 1) p)
      else
        h::(replace_player t (i - 1) p)

  (** [updated_player_list] is the list of players, but with player [p] 
      updated, according to state [s]. *)
  let update_player_list (s: state_spec) (p:Board.player)  = 
    let new_coins = p.coins + 10 in 
    let updated_player = {p with coins = new_coins} in
    let index_of_player = index p s.game_info.player_data 0 in 
    let new_player_list = replace_player s.game_info.player_data 
        index_of_player updated_player in
    new_player_list

  (** [get_opponent self lst] is the player that [self] chooses to challenge
      in a mini game. *)
  let get_opponent (self:Board.player) (lst:Board.player list) = 
    print_string ">";
    let input = read_line() in
    if input = self.name then raise InvalidChallenge else
      let rec get_opp' (inp:string) (lst2:Board.player list) = 
        match lst2 with 
        |[] -> raise UnknownPlayer
        |h::t -> if inp = h.name then h else get_opp' inp t
      in get_opp' input lst 

  (** [get_opp'' self lst] is the player that [self] chooses to challenge
      in a mini game. *)
  let rec get_opp'' self lst = 
    match get_opponent self lst with
    |p -> p
    |exception (InvalidChallenge) -> 
      print_endline "You can't challenge yourself! Choose another player."; 
      print_endline ""; get_opp'' self lst
    |exception (UnknownPlayer) -> 
      let name_list = (List.map (fun (p:Board.player) -> p.name) lst) in 
      let name_list_string = String.concat ", " name_list in
      print_endline ("That player does not exist. \
                      Choose another player from the following list: " ^
                     name_list_string); 
      print_endline ""; get_opp'' self lst

  (** [init g p] is the current state spec with game spec [g] and phrase list
      [p]. *)
  let init (g:Game.game_spec) (p:Game.phrase list) = 
    match p with 
    | t::(Game.Minigame m)::(Game.Player p)::[] -> {
        state_info = {
          game = m; 
          num_players = 
            if t = Game.SinglePlayer then 1 
            else if t = Game.TwoPlayer then 2 
            else failwith "unimplemented";
          triggered_by = p};
        game_info = g}
    | _ -> failwith "Unrecognized phrases at MinigameState"

  (** [determine_players s] is the amount of players that will play the
      minigame triggereed in state [s]. *)
  let determine_players s  = 
    match s.state_info.num_players with 
    |1 -> 1
    |2 -> 2
    |_ -> failwith "not possible"

  (** [single_player s] is the result of playing the single-player mini game 
      triggered in state [s]. *)
  let single_player s = 
    let game_result = 
      match s.state_info.game with 
      | Minigames.Typing -> Minigames.play_typing ()
      | Minigames.Multiplication -> Minigames.play_multiplication ()
      | Minigames.Trivia -> Minigames.play_trivia ()
      | Minigames.Scramble -> Minigames.play_scramble () 
      |_ -> failwith "not single-player game"
    in game_result

  (** [two_player s] is the result of player [p] and opponent [o] completing
      the two-player mini game triggered in state [s]. *)
  let two_player s (p:Board.player) (o:Board.player) = 
    let game_result = match s.state_info.game with 
      |Minigames.RPS -> Minigames.play_rps p.name o.name
      |_ -> failwith "not 2 person game"
    in game_result

  (** [new_spec s s_type p_data ps] is the new state spec after completing the 
      mini game, with type [s_type], player data [p_data], and phrases [ps]. *)
  let new_spec s s_type p_data ps = 
    Different  {
      next_state = s_type; 
      game_info = {
        player_data = p_data;
        board_data = s.game_info.board_data};
      phrases = ps
    }

  (** [play_single_player s player] is the new state spec after [player] 
      completes the single-player game in state [s]. *)
  let play_single_player s player = 
    let game_result = single_player s in
    (*player won the game *)
    if game_result = 1 then 
      let new_player_list = update_player_list s player in
      new_spec s State.Move new_player_list [Game.Player player; Game.SmallDie]
      (*player lost the game *)
    else new_spec s State.Turn s.game_info.player_data []

  (** [play_two_player s player] is the new state spec after [player] and
      their chosen opponent completes the two-player game in state [s]. *)
  let play_two_player s player = 
    (*player is p1 *)
    let players_list = s.game_info.player_data in
    (*opponent is p2 *)
    let opponent = get_opp'' player players_list in
    print_endline (player.name ^ " challenged " ^ opponent.name ^ "!");
    print_endline "";
    (*returns 2 if p1 wins, 3 if p2 wins, 4 if it's a draw *)
    let game_result = two_player s player opponent in
    if game_result = 2 then  
      (*p1 wins *)
      let new_player_list = update_player_list s player in
      new_spec s State.Turn new_player_list []
    else if game_result = 3 then 
      (*p2 wins *)
      let new_player_list = update_player_list s opponent in
      new_spec s State.Turn new_player_list []
      (*draw *) 
    else new_spec s State.Turn s.game_info.player_data []

  (** [eval s] is the new state spec after a mini game is completed in state
      [s]. *)
  let eval (s:state_spec) = 
    let player = s.state_info.triggered_by in
    match determine_players s with 
    | 1 ->
      print_endline (player.name ^ ", you've landed on a  minigame tile! \
                                    Win to roll again! \n");
      play_single_player s player
    | 2 -> 
      print_endline "You landed on a challenge tile! Enter the name of another \
                     player to challenge them to a game.";
      play_two_player s player
    | _ -> failwith "Invalid numbers of players!" end

