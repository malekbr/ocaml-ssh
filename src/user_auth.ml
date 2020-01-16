let service_name = "ssh-userauth"

let request_generic ~username ~method_ write_buffer =
  Write_buffer.message_id write_buffer Userauth_request;
  Write_buffer.string write_buffer username;
  Write_buffer.string write_buffer service_name;
  Write_buffer.string write_buffer method_
;;

let request_none = request_generic ~method_:"none"
