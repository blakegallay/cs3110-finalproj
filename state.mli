(** [stateType] is the type representing the different types of states that
    the game may be in at any given point in time. Expression of this type
    are passed as information to the main game loop to determine what 
    type of state should be initiated next.*)
type stateType = Turn | Move | Minigame

module type State = sig 

  (** [t] is the type representing the information that specifies the specifics
      of a state. i.e. the number of moves a player has left.*)
  type t

  (** [state_spec] is type representing the specific specification of the state
      in combination with the higher-level game specification, including player 
      and board data.*)
  type state_spec = {state_info: t; game_info: Game.game_spec}

  (** [out] is the type representing a state succeeding evaluation of the 
      current state. [Same] means that the same state will be returned to,
      [Different] means that a transition to another type of state will follow.
  *)
  type out = 
    |Same of state_spec 
    | Different of 
        {next_state: stateType; 
         game_info: Game.game_spec; phrases: Game.phrase list}

  (** [init g] is initial state specification given a game specification [g]*)
  val init : Game.game_spec -> Game.phrase list -> state_spec

  (** [eval s] is the information about the next state that will be
      initiated and evaluated following evaluation of the current state.*)
  val eval : state_spec -> out

end
