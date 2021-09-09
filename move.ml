(* [MoveState] represents the [State] where a player gets to roll a die and 
   then traverse tiles. *)
module MoveState : State.State = struct

  (** [roll_die range player_name] is the roll of a dice of [range] numbers for 
      [player_name]. *)
  let rec roll_die range player_name = 
    print_endline ("Type 'roll' to roll your die (1-" ^ string_of_int range ^ 
                   ")\n"
                  );
    print_string ">";
    match read_line () with
    | exception End_of_file -> 
      print_endline "I don't recognize that!\n"; roll_die range player_name
    | s -> if s = "roll" then let roll = 1 + Random.int range in 
        (print_endline (String.concat "" ["You rolled a ";string_of_int roll]));
        roll
      else ((print_endline "I don't recognize that!\n");
            roll_die range player_name)

  (** [t] is the type that keeps track of where the player is in their move. *)
  type t = {starting: bool; moves: int; player: string}

  type state_spec = {state_info: t; game_info:Game.game_spec}

  (** [out] is the type of the next move of the game. *)
  type out = Same of state_spec | Different of 
               { next_state: State.stateType;
                 game_info: Game.game_spec;
                 phrases: Game.phrase list
               }

  (** [init g phrases] is the initial move at the start of a player's turn. *)
  let init (g:Game.game_spec) (phrases:Game.phrase list) = 
    match phrases with
    | p::d::[] -> begin match p with 
        | Game.Player player -> 
          {state_info = {
              starting = true;
              moves = roll_die 
                  (match d with 
                   | Game.SmallDie -> 3 
                   | Game.MediumDie -> 5 
                   | Game.BigDie -> 7 | 
                     _ -> failwith "Unrecognized phrase") 
                  player.name; 
              player = player.name}; game_info = g}
        | _ -> failwith "unexpected phrase sent to Move" end
    | _ -> failwith "no phrases sent to Move"

  (** [eval s] is the result of a player's move. *)
  let eval (s:state_spec) =  
    Unix.sleepf 0.5;
    (* Clear the terminal screen and print the game board *)
    let _ = Sys.command "clear" in
    Board.print_board s.game_info.player_data s.game_info.board_data;

    let current_player = Board.find_player s.state_info.player 
        s.game_info.player_data in

    let current_tile = Board.find_tile current_player.tile 
        s.game_info.board_data in 
    (* Check if a planted bomb is located on the current tile. If one is,
       end the turn immediately. Otherwise proceed. *)
    if List.mem Board.PlantedBomb current_tile.structures 
    && not s.state_info.starting then (
      print_endline "[!!!] You triggered a plasma bomb! \
                     Your turn ends immediately! [!!!]\n";
      Unix.sleepf 0.5;
      Different 
        { next_state = State.Turn; 
          game_info = 
            { player_data = s.game_info.player_data;
              board_data = 
                Board.remove_structure PlantedBomb current_tile 
                  s.game_info.board_data};
          phrases = []
        } )
    else if (current_tile.number = 15) then 
      (let player_data = Board.do_store_interaction current_player current_tile 
           s.game_info.player_data in
       (* Check to see if the current player has enough stars to win the game. 
          Prints winning message if they do and carries on if not. *)
       (if (Board.winner current_player player_data) then 
          (print_endline ("Congrats! Player " ^ 
                          current_player.name ^ " has won the game!");
           exit 0)
        else ()
       );
       Different 
         { next_state = State.Turn; 
           game_info = {
             player_data = player_data;
             board_data = s.game_info.board_data};
           phrases = []
         })
    else 
      let move_count = s.state_info.moves in 
      if move_count > 0 then  
        Same {
          state_info={
            starting = false; moves=move_count-1; player=s.state_info.player
          }; 
          game_info = 
            { player_data = 
                Board.move s.game_info.player_data s.game_info.board_data 
                  s.state_info.player Board.Forwards; 
              board_data = s.game_info.board_data
            }
        } 
      else 
        let location = 
          Board.location s.game_info.player_data s.state_info.player
            s.game_info.board_data in

        print_endline (String.concat "" 
                         [s.state_info.player;" landed on tile #"; 
                          string_of_int location.number; "\n"]);
        let player_data = 
          Board.do_property_tax current_player current_tile 
            s.game_info.player_data 
          |> Board.do_store_interaction current_player current_tile 
          |> Board.do_coin_collection current_player current_tile in
        match Board.get_game location with
        | Empty -> Different 
                     {
                       next_state = State.Turn; 
                       game_info = {
                         player_data = player_data;
                         board_data = s.game_info.board_data};
                       phrases = []
                     }
        | Game m -> 
          let player_type = match m with 
            | Minigames.RPS -> Game.TwoPlayer
            | _ -> Game.SinglePlayer in
          Different 
            {
              next_state = State.Minigame;
              game_info = {
                player_data = player_data;
                board_data = s.game_info.board_data};
              phrases = [ player_type;
                          Game.Minigame m;
                          Game.Player 
                            (Board.find_player current_player.name player_data)
                        ]
            } 
end
