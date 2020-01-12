open! Core
open! Async

let test ~where =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port (Host_and_port.of_string where)
  in
  Ocaml_ssh.Transport.tcp_connection ~where_to_connect
;;

let () =
  Command.async
    (let open Command.Let_syntax in
    let%map_open where = anon ("where" %: string) in
    fun () -> test ~where)
    ~summary:""
  |> Command.run
;;
