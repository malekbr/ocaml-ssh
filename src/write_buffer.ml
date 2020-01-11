open! Core

type t = (read_write, Iobuf.seek) Iobuf.t

let max_size = 32768

let create () = Iobuf.create ~len:max_size

let byte = Iobuf.Fill.char

let bool t bool = Iobuf.Fill.uint8_trunc t (Bool.to_int bool)

let uint8 = Iobuf.Fill.uint8_trunc

let uint32 = Iobuf.Fill.uint32_be_trunc

let uint64 = Iobuf.Fill.uint64_be_trunc

let string t str =
  uint32 t (String.length str);
  Iobuf.Fill.stringo t str
;;

let bytes t str = Iobuf.Fill.stringo t str

let mpint_str z =
  if Z.equal z Z.zero then ""
  else
    let is_negative = Z.lt z Z.zero in
    let size = Z.numbits z in
    let size =
      if
        (is_negative && Z.trailing_zeros z < size)
        || ((not is_negative) && Z.testbit z (size - 1))
      then size + 1
      else size
    in
    let size = size + (-size % 8) in
    let size = size / 8 in
    let cstruct = Nocrypto.Numeric.Z.to_cstruct_be ~size z in
    Cstruct.to_string cstruct
;;

let%expect_test "mpint_str" =
  let mpint z =
    let s = mpint_str z in
    print_endline (String.Hexdump.to_string_hum s)
  in
  mpint Z.zero;
  [%expect {||}];
  mpint (Z.of_string "0x9a378f9b2e332a7");
  [%expect
    {| 00000000  09 a3 78 f9 b2 e3 32 a7                           |..x...2.| |}];
  mpint (Z.of_string "128");
  [%expect
    {| 00000000  00 80                                             |..| |}];
  mpint (Z.of_string "0xff");
  [%expect
    {| 00000000  00 ff                                             |..| |}];
  mpint (Z.of_string "0x7f");
  [%expect
    {| 00000000  7f                                                |.| |}];
  mpint (Z.of_string "-128");
  [%expect
    {| 00000000  ff 80                                             |..| |}];
  mpint (Z.of_string "256");
  [%expect
    {| 00000000  01 00                                             |..| |}];
  mpint (Z.of_string "-256");
  [%expect
    {| 00000000  ff 00                                             |..| |}];
  mpint (Z.of_string "0x80");
  [%expect
    {| 00000000  00 80                                             |..| |}];
  mpint (Z.of_string "-0x1234");
  [%expect
    {| 00000000  ed cc                                             |..| |}];
  mpint (Z.of_string "-0xdeadbeef");
  [%expect
    {| 00000000  ff 21 52 41 11                                    |.!RA.| |}]
;;

let mpint t z = string t (mpint_str z)

let name_list t strings = string t (String.concat ~sep:"," strings)

let consume_to_string t =
  Iobuf.flip_lo t;
  let result = Iobuf.Consume.stringo t in
  Iobuf.reset t;
  result
;;

let wrote t = Iobuf.Expert.lo t - Iobuf.Expert.lo_min t

let reset t = Iobuf.reset t

let read_buffer t = Read_buffer.create t
