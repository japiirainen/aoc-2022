open Base
open Stdio

module type Solver = sig
  val solve : string -> string * string
end

let read_all env filepath =
  let ( / ) = Eio.Path.( / ) in
  Eio.Path.load (Eio.Stdenv.cwd env / filepath)

let ( << ) = Fn.compose
let print_list list to_string = list |> List.iter ~f:(print_endline << to_string)