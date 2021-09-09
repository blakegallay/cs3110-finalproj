(** Minigame infrastructure and execution *)

type minigame = Typing | Multiplication | Scramble | Trivia | RPS

(** The type of the game result. *)
type game_result = int

(** [play_trivia] has the user play the trivia minigame. *)
val play_trivia : 'a -> game_result

(** [play_scramble] has the user play the unscramble minigame. *)
val play_scramble : 'a -> game_result

(** [play_typing] has the user play the typing minigame. *)
val play_typing : 'a -> game_result

(** [play_multiplication] has the user play the multiplication minigame. *)
val play_multiplication : 'a -> game_result

(** [play_rps] has the user play the rock, paper, scissors minigame. *)
val play_rps : string -> string -> game_result

