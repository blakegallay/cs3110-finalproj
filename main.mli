(** Contains initialization functions, is called when [make play] is run.*)

open State 

(** [Initializer] is the module that handles setting up the initial game state,
    and starting the main State-stepping loop*)
module Initializer (Init:State.State) : sig 

  (** [initGameData] is the initial game specification.*)
  val initGameData : Game.game_spec 

  (** [play g] starts the game that begins with the game specification [g]*)
  val play : Game.game_spec -> 'a

end