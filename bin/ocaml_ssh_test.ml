open! Core
open! Async
open! Ocaml_ssh

let test ~where ~username ~command:_ =
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
  (*
  let%bind result =
    Ocaml_ssh.Transport.request_keyboard_interactive connection ~username
      ~respond:(fun respond ->
        User_auth.Keyboard_authentication.Request.name respond
        |> printf "name: %s\n";
        User_auth.Keyboard_authentication.Request.instruction respond
        |> printf "instruction: %s\n";
        User_auth.Keyboard_authentication.Request.respond respond
          (fun ~prompt ~echo ->
            print_string prompt;
            Readline.read_line ~echo))
  in
  *)
  print_s [%message (result : Ocaml_ssh.User_auth.Auth_result.t)];
  let%bind channel = Ocaml_ssh.Transport.request_channel connection in
  print_s [%message (channel : Ocaml_ssh.Channel.t)];
  (*
  let%bind response, exit_code =
    Ocaml_ssh.Channel.request_exec channel ~command
  in
  print_endline response;
  exit (Option.value_map ~default:0 exit_code ~f:Unix.Exit.code)
  *)
  let%bind () =
    Channel.request_pty channel ~term:"vt100" ~width:80 ~height:120 >>| ok_exn
  in
  let%bind data_pipe, stderr, std_in =
    Channel.request_shell channel >>| ok_exn
  in
  Reader.transfer (Lazy.force Reader.stdin) std_in |> don't_wait_for;
  let%bind () = Pipe.iter_without_pushback data_pipe ~f:print_string
  and () =
    Pipe.iter_without_pushback stderr
      ~f:(Writer.write (Lazy.force Writer.stderr))
  in
  Transport.closed connection
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
