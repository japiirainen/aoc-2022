open Core
open Fn
open Lib

let sum_cals n input =
  let sum = List.fold ~init:0 ~f:( + ) in
  Str.split (Str.regexp "\n\n") input
  |> List.map ~f:(sum << List.map ~f:int_of_string << String.split ~on:'\n')
  |> (sum << flip List.take n << (List.rev << List.sort ~compare))

let solve input =
  (string_of_int @@ sum_cals 1 input, string_of_int @@ sum_cals 3 input)
