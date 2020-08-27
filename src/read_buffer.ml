open! Core
open! Import

type t = (read, Iobuf.seek) Iobuf.t

let create (iobuf : ([> read ], Iobuf.seek) Iobuf.t) : t =
  Iobuf.sub_shared (iobuf :> (read, Iobuf.seek) Iobuf.t)
;;

let bytes t ~len = Iobuf.Consume.stringo ~len t

let byte = Iobuf.Consume.char

let uint8 = Iobuf.Consume.uint8

let uint16 = Iobuf.Consume.uint16_be

let uint32 = Iobuf.Consume.uint32_be

let uint64 = Iobuf.Consume.uint64_be_exn

let message_id t = uint8 t |> Message_id.of_code

let peek t = Iobuf.Peek.stringo t ~pos:0

let string t =
  let len = uint32 t in
  bytes t ~len
;;

let name_list t = string t |> String.split ~on:','

let mpint t =
  let str = string t in
  let is_non_negative =
    String.is_empty str || Char.(str.[0] < of_int_exn 0x80)
  in
  let initial =
    Cstruct.of_string str |> Mirage_crypto_pk.Z_extra.of_cstruct_be
  in
  if is_non_negative then initial
  else Z.(shift_left minus_one (String.length str) |> logor initial)
;;

let%expect_test "mpint deserializes correctly" =
  let buffer = Iobuf.create ~len:256 in
  let t = create buffer in
  let test s =
    Iobuf.Fill.uint32_be_trunc buffer (String.length s);
    Iobuf.Fill.stringo buffer s;
    mpint t |> Z.to_string |> print_endline
  in
  test "";
  [%expect {|0|}];
  test "\xff\xff";
  [%expect {|-1|}];
  test "\x7f";
  [%expect {|127|}];
  test "\x00\xff";
  [%expect {|255|}]
;;

let read t = Iobuf.Expert.lo t - Iobuf.Expert.lo_min t

let bool t = uint8 t > 0

let narrow t len =
  let other = create t in
  Iobuf.resize other ~len;
  other
;;
