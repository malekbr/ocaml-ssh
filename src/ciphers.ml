open! Core

type t =
  | Three_des_cbc
  | Blowfish_cbc
  | Twofish256_cbc
  | Twofish_cbc
  | Twofish192_cbc
  | Twofish128_cbc
  | Aes256_cbc
  | Aes256_ctr
  | Aes192_cbc
  | Aes128_cbc
  | Serpent256_cbc
  | Serpent192_cbc
  | Serpent128_cbc
  | Arcfour
  | Idea_cbc
  | Cast128_cbc
[@@deriving enumerate]

let to_string = function
  | Three_des_cbc -> "3des-cbc"
  | Blowfish_cbc -> "blowfish-cbc"
  | Twofish256_cbc -> "twofish256-cbc"
  | Twofish_cbc -> "twofish-cbc"
  | Twofish192_cbc -> "twofish192-cbc"
  | Twofish128_cbc -> "twofish128-cbc"
  | Aes256_cbc -> "aes256-cbc"
  | Aes256_ctr -> "aes256-ctr"
  | Aes192_cbc -> "aes192-cbc"
  | Aes128_cbc -> "aes128-cbc"
  | Serpent256_cbc -> "serpent256-cbc"
  | Serpent192_cbc -> "serpent192-cbc"
  | Serpent128_cbc -> "serpent128-cbc"
  | Arcfour -> "arcfour"
  | Idea_cbc -> "idea-cbc"
  | Cast128_cbc -> "cast128-cbc"
;;

let of_string = function
  | "3des-cbc" -> Some Three_des_cbc
  | "blowfish-cbc" -> Some Blowfish_cbc
  | "twofish256-cbc" -> Some Twofish256_cbc
  | "twofish-cbc" -> Some Twofish_cbc
  | "twofish192-cbc" -> Some Twofish192_cbc
  | "twofish128-cbc" -> Some Twofish128_cbc
  | "aes256-cbc" -> Some Aes256_cbc
  | "aes192-cbc" -> Some Aes192_cbc
  | "aes128-cbc" -> Some Aes128_cbc
  | "serpent256-cbc" -> Some Serpent256_cbc
  | "serpent192-cbc" -> Some Serpent192_cbc
  | "serpent128-cbc" -> Some Serpent128_cbc
  | "arcfour" -> Some Arcfour
  | "idea-cbc" -> Some Idea_cbc
  | "cast128-cbc" -> Some Cast128_cbc
  | "aes256-ctr" -> Some Aes256_ctr
  | _ -> None
;;
