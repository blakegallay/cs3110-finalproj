(** Contains a [State] where a player gets to roll a die and 
    then traverse tiles.*)

open State
open Board

(** [MoveState] is the [State] where a player gets to roll a die and 
    then traverse tiles.*)
module MoveState : State.State