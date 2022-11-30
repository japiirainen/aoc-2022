open Base
open Stdio

module type Solver = sig
  val solve : string List.t -> string * string
end

module Result_syntax = struct
  let ( let* ) x f = Result.bind ~f x
  let return = Result.return
end

module Option_syntax = struct
  let ( let* ) x f = Option.bind ~f x
  let return = Option.return
end

let read_lines filepath =
  try Ok (In_channel.read_lines filepath) with _ -> Error "File not found"

let print_list list to_string =
  list |> List.iter ~f:(Fn.compose print_endline to_string)