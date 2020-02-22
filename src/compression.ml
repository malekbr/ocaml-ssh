module type S = sig
  type t

  val name : string

  val compress : t -> string -> string

  val decompress : t -> string -> string

  val create : authenticated:(unit -> bool) -> t
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

        let create ~authenticated:_ = ()
      end )
  ;;

  (* TODO finish *)
  let zlib_openssh : t =
    T
      ( module struct
        type t = unit -> bool

        let name = "zlib@openssh.com"

        let compress _ s = s

        let decompress _ s = s

        let create ~authenticated = authenticated
      end )
  ;;

  let name (T (module M)) = M.name

  let create (T ((module M) as t)) ~authenticated =
    State (M.create ~authenticated, t)
  ;;

  let all = [ none ]
end

let compress (State (t, (module M))) = M.compress t

let decompress (State (t, (module M))) = M.decompress t
