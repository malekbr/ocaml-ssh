open! Core
open! Async

let read_line ~echo =
  let%bind () =
    if echo then Deferred.unit
    else
      let%bind t = Unix.Terminal_io.tcgetattr (Fd.stdin ()) in
      t.c_echo <- false;
      Unix.Terminal_io.tcsetattr t (Fd.stdin ()) ~mode:TCSANOW
  in
  let%bind input = Reader.read_line (Lazy.force Reader.stdin) in
  let input =
    match input with `Ok password -> password | `Eof -> assert false
  in
  let%map () =
    if echo then Deferred.unit
    else
      let%bind t = Unix.Terminal_io.tcgetattr (Fd.stdin ()) in
      t.c_echo <- true;
      let%map () = Unix.Terminal_io.tcsetattr t (Fd.stdin ()) ~mode:TCSANOW in
      print_endline ""
  in
  input
;;

let yes_or_no () =
  print_string "[y/yes/anything else is no]";
  let%map result = read_line ~echo:true in
  match String.lowercase result with "yes" | "y" -> true | _ -> false
;;

let read_password ?(prompt = "Password: ") () =
  print_string prompt;
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
  return password
;;
