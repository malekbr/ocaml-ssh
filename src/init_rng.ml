open! Core
open! Async

let devices = [ "/dev/urandom"; "/dev/random" ]

let a_little = 32

let a_lot = 1024

let sys_rng () =
  let%map device =
    Deferred.List.find devices ~f:(fun device ->
        match%map Sys.file_exists device with
        | `Yes -> true
        | `No | `Unknown -> false)
  in
  Option.value_exn device
;;

let reseed ?(size = a_little) ?(device = sys_rng) () =
  let%bind device = device () in
  Reader.with_file device ~f:(fun reader ->
      let bytes = Bytes.create size in
      match%map Reader.really_read reader bytes with
      | `Eof _ -> raise_s [%message "Failed to read enough entropy"]
      | `Ok -> Nocrypto.Rng.reseed (Cstruct.of_bytes bytes))
;;
