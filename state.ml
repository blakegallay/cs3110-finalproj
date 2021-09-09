type stateType = Turn | Move | Minigame

module type State = sig 

  type t

  type state_spec = {state_info: t; game_info: Game.game_spec}

  type out = 
    | Same of state_spec 
    | Different of 
        {next_state: stateType; 
         game_info: Game.game_spec; phrases: Game.phrase list}

  val init : Game.game_spec -> Game.phrase list -> state_spec

  val eval : state_spec -> out

end
