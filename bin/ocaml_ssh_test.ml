open! Core
open! Async

let test ~where ~username ~command =
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
  let%bind password = Readline.read_password () in
  let%bind result =
    Ocaml_ssh.Transport.request_password connection ~username ~password
  in
  print_s [%message (result : Ocaml_ssh.User_auth.Auth_response.t)];
  let%bind channel = Ocaml_ssh.Transport.request_channel connection in
  print_s [%message (channel : Ocaml_ssh.Channel.t)];
  let%bind response, exit_code =
    Ocaml_ssh.Channel.request_exec channel ~command
  in
  print_endline response;
  exit (Option.value_map ~default:0 exit_code ~f:Unix.Exit.code)
;;

let () =
  Command.async
    (let open Command.Let_syntax in
    let%map_open where = anon ("where" %: string)
    and username = flag "user" (optional string) ~doc:"username"
    and command = flag "command" (required string) ~doc:"command" in
    fun () -> test ~username ~where ~command)
    ~summary:""
  |> Command.run
;;
