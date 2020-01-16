open! Core
open! Async

let test ~where ~username =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port (Host_and_port.of_string where)
  in
  let%bind username =
    match username with
    | Some username -> return username
    | None -> Unix.getlogin ()
  in
  let%bind connection = Ocaml_ssh.Transport.create ~where_to_connect in
  let%bind () = Ocaml_ssh.Transport.request_auth connection in
  Ocaml_ssh.Transport.request_userauth_list connection ~username;
  Ocaml_ssh.Transport.closed connection
;;

let () =
  Command.async
    (let open Command.Let_syntax in
    let%map_open where = anon ("where" %: string)
    and username = flag "user" (optional string) ~doc:"username" in
    fun () -> test ~username ~where)
    ~summary:""
  |> Command.run
;;
