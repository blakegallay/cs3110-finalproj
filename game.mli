(** 
   Defines various types which are used to keep track of the state of the game.
*)

(** The type [player_data] represents information about the players. *)
type player_data = Board.player list

(** The type [board_data] represents information about the board. *)
type board_data = Board.board

(** The type [game_spec] represents information about the players and
    board. *)
type game_spec = {player_data: player_data; board_data: board_data;}

(** [init_player_data x] *)
val init_player_data : unit -> player_data

(** [init_board_data x] is the board of the game. *)
val init_board_data : unit -> board_data

(** The type [phrase] represents the different phrases that can be used for
    the parts of the game. *)
type phrase = Player of Board.player
            | Minigame of Minigames.minigame
            | StandardTurn | MinigameLose | MinigameWin
            | SinglePlayer | TwoPlayer
            | SmallDie | MediumDie | BigDie
