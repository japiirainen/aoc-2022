open Core
open Lib

let solvers : (module Solver) Array.t = [| (module Day01) |]

let parse_args (args : string array) =
  match Array.to_list args with
  | _ :: day :: files -> (int_of_string day, files)
  | _ -> failwith "invalid arguments provided."

let runner env =
  let day, files = parse_args (Sys.get_argv ()) in
  let (module Solver) = solvers.(day - 1) in
  List.map files ~f:(fun fp ->
      let part1, part2 = Solver.solve (Lib.read_all env fp) in
      Printf.sprintf "\nFilepath: %s\nPart 1: %s\nPart 2: %s" fp part1 part2)

let () =
  Eio_main.run @@ fun env ->
  match runner env |> List.iter ~f:print_endline with
  | () -> ()
  | exception _ -> print_endline "\nUsage: ./a.out <day> <file1> <file2> ..."
