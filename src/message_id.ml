open! Core

type t =
  | Disconnect
  | Ignore
  | Unimplemented
  | Debug
  | Service_request
  | Service_accept
  | Key_exchange_init
  | New_keys
  | Key_exchange_algorithm_specific of int
  | Userauth_request
  | Userauth_failure
  | Userauth_success
  | Userauth_banner
  | Userauth_algorithm_specific of int
  | Global_request
  | Request_success
  | Request_failure
  | Channel_open
  | Channel_open_confirmation
  | Channel_open_failure
  | Channel_window_adjust
  | Channel_data
  | Channel_extended_data
  | Channel_eof
  | Channel_close
  | Channel_request
  | Channel_success
  | Channel_failure
[@@deriving sexp_of, equal]

let of_code = function
  | 1 -> Disconnect
  | 2 -> Ignore
  | 3 -> Unimplemented
  | 4 -> Debug
  | 5 -> Service_request
  | 6 -> Service_accept
  | 20 -> Key_exchange_init
  | 21 -> New_keys
  | i when i >= 30 && i < 50 -> Key_exchange_algorithm_specific i
  | 50 -> Userauth_request
  | 51 -> Userauth_failure
  | 52 -> Userauth_success
  | 53 -> Userauth_banner
  | i when i >= 60 && i < 80 -> Userauth_algorithm_specific i
  | 80 -> Global_request
  | 81 -> Request_success
  | 82 -> Request_failure
  | 90 -> Channel_open
  | 91 -> Channel_open_confirmation
  | 92 -> Channel_open_failure
  | 93 -> Channel_window_adjust
  | 94 -> Channel_data
  | 95 -> Channel_extended_data
  | 96 -> Channel_eof
  | 97 -> Channel_close
  | 98 -> Channel_request
  | 99 -> Channel_success
  | 100 -> Channel_failure
  | i -> raise_s [%message "Unimplemented" (i : int)]
;;

let to_code = function
  | Disconnect -> 1
  | Ignore -> 2
  | Unimplemented -> 3
  | Debug -> 4
  | Service_request -> 5
  | Service_accept -> 6
  | Key_exchange_init -> 20
  | New_keys -> 21
  | Userauth_request -> 50
  | Userauth_failure -> 51
  | Userauth_success -> 52
  | Userauth_banner -> 53
  | Global_request -> 80
  | Request_success -> 81
  | Request_failure -> 82
  | Channel_open -> 90
  | Channel_open_confirmation -> 91
  | Channel_open_failure -> 92
  | Channel_window_adjust -> 93
  | Channel_data -> 94
  | Channel_extended_data -> 95
  | Channel_eof -> 96
  | Channel_close -> 97
  | Channel_request -> 98
  | Channel_success -> 99
  | Channel_failure -> 100
  | Key_exchange_algorithm_specific i | Userauth_algorithm_specific i -> i
;;
