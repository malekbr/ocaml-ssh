open! Core
open! Async

let service_name = "ssh-userauth"

let request_service_name = "ssh-connection"

module Auth_response = struct
  type t = Accepted | Refused of { alternatives : string list }
  [@@deriving sexp_of]
end

type t = Auth_response.t Mvar.Read_write.t

let create = Mvar.create

let request_generic t ~username ~method_ write_buffer =
  if not (Mvar.is_empty t) then
    raise_s [%message "Did not receive response for last auth request"];
  Write_buffer.message_id write_buffer Userauth_request;
  Write_buffer.string write_buffer username;
  Write_buffer.string write_buffer request_service_name;
  Write_buffer.string write_buffer method_;
  Mvar.take t
;;

let request_none = request_generic ~method_:"none"

let request_password ~password t ~username write_buffer =
  let result = request_generic t ~username ~method_:"password" write_buffer in
  Write_buffer.bool write_buffer false;
  Write_buffer.string write_buffer password;
  result
;;

let register_denied t read_buffer =
  let alternatives = Read_buffer.name_list read_buffer in
  Mvar.set t (Auth_response.Refused { alternatives })
;;

let register_accepted t = Mvar.set t Auth_response.Accepted
