open! Core

let ssh_version = "2.0"

let software_version = "0.1"

let implementation = sprintf "ocaml_ssh_%s" software_version

let identification_string =
  sprintf "SSH-%s-%s nocomment" ssh_version implementation
;;
