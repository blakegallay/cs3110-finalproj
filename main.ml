(** Contains initialization functions, is called when [make play] is run.*)

open Game

exception Quit

module Initializer (Init:State.State) = struct

  let initGameData : Game.game_spec = 
    let pd = Game.init_player_data () in 
    let bd = Game.init_board_data () in 
    {player_data=pd; board_data=bd}

  let play (g:Game.game_spec) = let module Stepper_functor (S:State.State) = struct 
                                  let rec step (s:S.state_spec) =
                                    match S.eval s with 
                                    | S.Same new_spec -> step new_spec
                                    | S.Different 
                                        {next_state=new_state; 
                                         game_info=new_spec; phrases=phrases} -> 
                                      (new_state, new_spec, phrases)
                                end in 

    let module Stepper = Stepper_functor(Init) in 

    let rec step_cycle = function 
      | (st, sp, p) -> begin match st with 
          | State.Turn -> let module Stepper = Stepper_functor(Turn.TurnState) 
            in step_cycle (Stepper.step (Turn.TurnState.init sp p))
          | State.Move -> let module Stepper = Stepper_functor(Move.MoveState) 
            in step_cycle (Stepper.step (Move.MoveState.init sp p))
          | State.Minigame -> let module Stepper = 
                                Stepper_functor(MinigameState.MinigameState) in 
            step_cycle (Stepper.step (MinigameState.MinigameState.init sp p))

        end in step_cycle (Stepper.step (Init.init g []))
end

(** [play_game f] starts the game*)
let play_game f =
  if f <> "enter" then raise End_of_file else
    print_endline "";   
  let module Initializer = Initializer(Turn.TurnState) in
  Initializer.play Initializer.initGameData

(**[enter_game i] checks that the user typed 'enter' to start the game. *)
let rec enter_game i = 
  if i = "enter" then play_game i else  print_endline ("Type 'enter' to begin.\n");
  print_string  "> ";
  let i' = read_line () in 
  enter_game i'

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our fire 3110 final project.\n");
  print_endline "Type 'enter' to begin.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> enter_game input

(** Execute the game engine. *)
let () = main ()