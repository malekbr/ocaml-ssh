open! Core
open! Async

let read_password () =
  print_string "Password: ";
  let%bind () = Writer.flushed (Lazy.force Writer.stdout) in
  let%bind t = Unix.Terminal_io.tcgetattr (Fd.stdin ()) in
  t.c_echo <- false;
  let%bind () = Unix.Terminal_io.tcsetattr t (Fd.stdin ()) ~mode:TCSANOW in
  let%bind password = Reader.read_line (Lazy.force Reader.stdin) in
  let%bind () = Writer.flushed (Lazy.force Writer.stdout) in
  let password =
    match password with `Ok password -> password | `Eof -> assert false
  in
  t.c_echo <- true;
  let%bind () = Unix.Terminal_io.tcsetattr t (Fd.stdin ()) ~mode:TCSANOW in
  print_endline "";
  return password
;;
