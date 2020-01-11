open! Core
open! Async

let test () =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port
      (Host_and_port.of_string "localhost:22")
  in
  Ocaml_ssh.Transport.tcp_connection ~where_to_connect
;;

let () = Command.async (Command.Param.return test) ~summary:"" |> Command.run
