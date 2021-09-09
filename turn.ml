(* [TurnState] represents the state where players are taking turns
   making rolling dice and making moves. *)
module TurnState : State.State = struct

  (* [t] is the next player in the turn queue. *)
  type t = unit

  type state_spec = {state_info: t; game_info: Game.game_spec;}

  type out = Same of state_spec | Different of 
               {
                 next_state: State.stateType;
                 game_info: Game.game_spec;
                 phrases: Game.phrase list
               }

  let init (g:Game.game_spec) (p:Game.phrase list) = 
    { state_info = ();
      game_info=g;
    }

  (** [queue_all players] is the [player list] with all players added
      to the move queue.*)
  let rec queue_all players = 
    match 
      List.find_opt (fun p -> not (Board.in_queue p)) players with
    | None -> players
    | Some p -> queue_all (Board.queue players p.name)

  (** [item_dialogue spec player] is the [game_spec] after [player] is given
      the option to use an item, if they have one or more in their inventory.*)
  let rec item_dialogue spec player : Game.game_spec =
    print_endline "You have items in your inventory! Enter the name of an \
                   item to use it, or 'move' to start your move."; 
    print_string  "> ";
    match read_line () with
    | "move" -> spec
    | s -> begin 
        match List.find_opt 
                (fun i -> 
                   String.uppercase_ascii (Board.string_of_item i) = 
                   (String.uppercase_ascii s)) player.Board.inventory with 
        | None -> print_endline "That's not a valid command/item.\n"; 
          item_dialogue spec player
        | Some i -> 
          let (updated_player_data, updated_board_data, used_item) = 
            Board.use_item player i spec.player_data spec.board_data 
          in 
          if used_item then 
            { player_data= updated_player_data;
              board_data = updated_board_data
            }
          else item_dialogue spec player
      end 

  (** [print_inv player] prints [player]'s inventory, and returns [()]*)
  let print_inv player = if player.Board.inventory = [] then 
      print_endline (String.concat "" ["(None)\n"]) else 
      let rec print_inv inv = match inv with 
        | [] -> ()
        | h::t -> ANSITerminal.
                    (print_string [white]
                       (Board.string_of_item h)
                    ); 
          print_endline ""; print_inv t
      in print_inv player.Board.inventory

  let eval (s:state_spec) = 

    Unix.sleepf 0.75;

    Board.print_board s.game_info.player_data s.game_info.board_data;

    (* If the move queue is empty, start the turn cycle over again.*)
    if List.filter (fun p -> Board.in_queue p) s.game_info.player_data = [] 
    then Same 
        { state_info = (); 
          game_info = 
            { player_data = queue_all s.game_info.player_data;
              board_data=s.game_info.board_data
            }
        }
    else 
      (* Next player in the move queue, whose turn it is now *)
      let next_plyr = 
        List.find (fun p -> Board.in_queue p) (s.game_info.player_data) in

      print_endline (String.concat "" 
                       ["-----------------------------------------------------\
                         \n";"";next_plyr.name;", it's your turn to move!\n"
                       ]);
      ANSITerminal.(print_string [cyan]
                      ("~~~~~~~~~~~~~~~~\n"); 
                    print_string [yellow] "Available Items:\n"); 
      print_inv next_plyr;
      ANSITerminal.(print_string [cyan]
                      "~~~~~~~~~~~~~~~~\n");
      print_endline (String.concat "" 
                       ["COINS: ";
                        string_of_int next_plyr.coins; "\n"]);
      print_endline (String.concat "" 
                       ["STARS: ";
                        string_of_int next_plyr.star_count; "\n"]);                     

      (* Allow the player to use an item, if their inventory is not empty *)
      let updated_spec = if next_plyr.inventory = [] then s.game_info else 
          (item_dialogue s.game_info next_plyr) in

      Different {
        next_state= State.Move; 
        game_info= {
          player_data = Board.dequeue updated_spec.player_data next_plyr.name; 
          board_data=updated_spec.board_data};
        phrases = [Game.Player next_plyr; Game.MediumDie]
      }
end
