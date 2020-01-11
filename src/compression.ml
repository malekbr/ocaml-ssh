module type S = sig
  type t

  val name : string

  val compress : t -> string -> string

  val decompress : t -> string -> string

  val create : unit -> t
end

type t = State : 'a * (module S with type t = 'a) -> t

module Method = struct
  type t = T : (module S with type t = 'a) -> t

  let none : t =
    T
      ( module struct
        type t = unit

        let name = "none"

        let compress () s = s

        let decompress () s = s

        let create () = ()
      end )
  ;;

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) = State (M.create (), t)

  let all = [ none ]
end

let compress (State (t, (module M))) = M.compress t

let decompress (State (t, (module M))) = M.decompress t
