type t

val create : unit -> t

val max_size : int

val byte : t -> char -> unit

val bool : t -> bool -> unit

val uint8 : t -> int -> unit

val uint32 : t -> int -> unit

val uint64 : t -> int -> unit

val string : t -> string -> unit

val bytes : t -> string -> unit
(** No size *)

val mpint : t -> Z.t -> unit

val name_list : t -> string list -> unit

val consume_to_string : t -> string

val read_buffer : t -> Read_buffer.t

val reset : t -> unit

val wrote : t -> int
