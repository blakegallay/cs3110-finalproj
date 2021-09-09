(** Constructs and handles the game board. *)

(** Raised when an invalid player name is entered. *)
exception Invalid_Player

(** Raised when a player tries to reach an invalid tile. *)
exception Invalid_Tile

(**  The type [direction] represents the direction the player will move
     next in the game. *)
type direction = Forwards | Backwards

(** The type [game] represents a game on a tile. A game can either be empty
    or the name of a minigame. *)
type game = Empty | Game of Minigames.minigame

(** The type [item] represents items of the game. *)
type item = Teleporter | Bomb | Deed of string 

(** The type [structure] represents structures which may be placed on
    tiles by players during the course of a game.. *)
type structure = PlantedBomb | Property of string * string

(** The type [store] represents a store on the board. *)
type store = Empty_Store | Store of (item*int) list | Star_Store 

(** The type [tile] represents a tile on the board. *)
type tile =
  { 
    number : int;
    game : game;
    next : int list;
    structures: structure list;
    store : store;
    tile_coins: int 
  }

(** The type [player] represents a player in the game. *)
type player = 
  {
    name : string;
    id: int;
    tile : int;
    in_move_queue : bool;
    inventory: item list;
    coins : int;
    star_count : int
  }

(** The type [board] represents the gameboard. *)
type board = tile list

(** [transfer_coins giver receiver players amount] is the [player list] with 
    [amount] coins transfered from [giver] to [receiver] *)
val transfer_coins : string -> string -> player list -> int -> player list

(** [string_of_item it] is a string representation of [it]. *)
val string_of_item : item -> string

(** [string_of_structure st] is a string representation of [st]. *)
val string_of_structure : structure -> string

(** [use_item user item players board] is the result of [user] using [item]. *)
val use_item : player -> item -> player list -> board -> 
  (player list * board * bool)

(** [remove_structure st ti board] removes the [st] at [ti]. *)
val remove_structure : structure -> tile -> board -> board

(** [construct size games] constructs a gameboard of [size] tiles and with [games] 
    as minigames. 
    TODO: should be tile list. *) 
val construct_board : Minigames.minigame list -> board

(** [construct_players old_players new_players] initializes a [new_players] list
    with all of the players from [old_players] at tile 1. 
    [old_players] is a list of the names of players in the game. *)
val construct_players : string list -> player list

(** [location players player_name] is the tile number that [player_name] is on. *)
val location : player list -> string -> board -> tile

(** [find_player name players] is the player corresponding to [name] in 
    [players]. *)
val find_player : string -> player list -> player

(**  [find_tile tile board] is the tile 
     on [board] corresponding to the number of [tile].*)
val find_tile : int -> board -> tile

(** [players_on_tile players tile] is a [player list] of all players currently
    located at tile [tile] on the game board. *)
val players_on_tile : player list -> tile -> player list

(** [next_tile player direction board] is the next tile that [player] will
    move to. *)
val next_tile : player -> direction -> board -> tile

(** [dequeue players player] is the [player list] with [player] set
    to no longer be in the move queue *)
val dequeue : player list -> string -> player list

(** [queue players player] is the [player list] with [player] set
    to be in the move queue *)
val queue : player list -> string -> player list

(** [in_queue player] is [true] if [player] is currently in the move queue,
    [false] otherwise. *)
val in_queue : player -> bool

(** [next_in_queue players] is the player of [players] whose turn is next. *)
val next_in_queue : player list -> player option

(** [move players player dir] is the [player list] containing all players
    in [players], with [player]'s tile location moved one tile in a direction.*)
val move : player list -> board -> string -> direction -> player list

(** [get_game tile] is the game associated with [tile]. *)
val get_game : tile -> game

(** [store_transaction tile player] is the updated inventory and coins of
    [player] after they visit the store of [tile]. *)
val store_transaction : tile -> player -> player

(** [winner player players] is [true] is [player] has won the game and [false]
    otherwise. *)
val winner : player -> player list -> bool

(** [collect_coins player tile players] is the updated [players] list after 
    [player] has collected the coins from [tile]. *)
val do_coin_collection : player -> tile -> player list -> player list

(** [star_transaction tile player] is the updated star_count of [player] after
    they visit the star store of [tile]. *)
val star_transaction : tile -> player -> player

(** [do_store_transaction player tile players] is the updated list of 
    [players] after the store transaction of [player] on [tile]. *)
val do_store_interaction : player -> tile -> player list -> player list

(** [do_property_tax player tile players] is the updated [players] after
    [player] pays the property tax on [tile]. *)
val do_property_tax : player -> tile -> player list -> player list

(** [print_board players board] prints a string representation of the 
    game board [board], and returns [()]. *)
val print_board : player list -> board -> unit