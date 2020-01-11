open! Core
open! Import

type t = {
    mutable buffer : Bigstring.t [@sexp.opaque]
  ; mutable pos : int
  ; mutable len : int
}
[@@deriving sexp_of]

let default_length = 4096

let create () = { buffer = Bigstring.create default_length; pos = 0; len = 0 }

let request t len =
  if t.len < len then None
  else
    let result = Bigstring.To_string.sub t.buffer ~pos:t.pos ~len in
    t.pos <- t.pos + len;
    t.len <- t.len - len;
    Some result
;;

let move_to_beginning t =
  Bigstring.blit ~src:t.buffer ~src_pos:t.pos ~dst:t.buffer ~dst_pos:0
    ~len:t.len;
  t.pos <- 0
;;

let double_size t =
  let buffer = Bigstring.create (Bigstring.length t.buffer * 2) in
  Bigstring.blit ~src:t.buffer ~src_pos:t.pos ~dst:buffer ~dst_pos:0 ~len:t.len;
  t.buffer <- buffer;
  t.pos <- 0
;;

let store t s =
  let len = String.length s in
  let new_len = t.len + len in
  if t.pos + new_len <= Bigstring.length t.buffer then
    (* we don't have to do anything in this scenario, we can just write *)
    ()
  else if new_len <= Bigstring.length t.buffer then move_to_beginning t
  else double_size t;
  Bigstring.From_string.blit ~src_pos:0 ~dst_pos:(t.pos + t.len) ~len
    ~dst:t.buffer ~src:s;
  t.len <- new_len
;;
