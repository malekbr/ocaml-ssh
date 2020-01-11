open! Core
open! Import

let generate_mac_string_to_verify ~sequence_number ~clean_work_buffer message =
  Write_buffer.uint32 clean_work_buffer (Uint32.to_int sequence_number);
  Write_buffer.bytes clean_work_buffer message;
  Write_buffer.consume_to_string clean_work_buffer
;;
