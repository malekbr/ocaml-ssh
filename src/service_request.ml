open! Core
open! Async

module Service = struct
  include String
end

module Req_result = struct
  type t = Accepted | Refused [@@deriving sexp_of]
end

module Response_queue = struct
  type t = unit Ivar.t Queue.t Service.Table.t

  let create () = Service.Table.create ()

  let add (t : t) ~service_name =
    let response = Ivar.create () in
    Hashtbl.update t service_name ~f:(function
      | None -> Queue.singleton response
      | Some queue ->
          Queue.enqueue queue response;
          queue);
    Ivar.read response
  ;;

  (* TODO close all on disconnect *)
  let respond (t : t) ~service_name =
    Hashtbl.change t service_name ~f:(function
      | None -> raise_s [%message "Unqualified response"]
      | Some queue ->
          let ivar = Queue.dequeue_exn queue in
          Ivar.fill ivar ();
          Option.some_if (Queue.is_empty queue |> not) queue)
  ;;
end

let request response_queue write_buffer ~service_name =
  Write_buffer.message_id write_buffer Service_request;
  Write_buffer.string write_buffer service_name;
  Response_queue.add response_queue ~service_name
;;

let respond response_queue read_buffer =
  let service_name = Read_buffer.string read_buffer in
  Response_queue.respond response_queue ~service_name
;;
