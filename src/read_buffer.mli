open! Core
open! Import

type t

val create : ([> read ], Iobuf.seek) Iobuf.t -> t

val bytes : t -> len:int -> string

val byte : t -> char

val uint8 : t -> int

val uint16 : t -> int

val uint32 : t -> int

val uint64 : t -> int

val message_id : t -> Message_id.t

val string : t -> string

val name_list : t -> string list

val mpint : t -> Z.t

val read : t -> int

val narrow : t -> int -> t

val bool : t -> bool
