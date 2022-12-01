open Core
open Lib

let solvers : (module Solver) Array.t =
  [|(module Day01) |]

let parse_args (args : string array) =
  match Array.to_list args with
  | _ :: day :: files -> Ok (day, files)
  | _ -> Error "Invalid arguments"

let runner () = 
  let open Lib.Result_syntax in
  let* day, files = parse_args (Sys.get_argv ()) in
  match solvers.(int_of_string day - 1) with
  | (exception Invalid_argument _)
  | (exception Failure _) ->
    print_endline @@ "Invalid day provided: " ^ day;
    exit 1
  | (module Solver) ->
    List.map files ~f:(fun fp ->
      let* lines = Lib.read_all fp in  
      let (part1, part2) = Solver.solve lines in
      return (Printf.sprintf "\nFilepath: %s\nPart 1: %s\nPart 2: %s" fp part1 part2))
    |> Result.all

let () =
  match runner () with
  | Ok result -> List.iter result ~f:print_endline
  | Error err -> prerr_endline err
