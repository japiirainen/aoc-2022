open Core
open Fn

let solve input =
  let sum = List.fold_left ~f:( + ) ~init:0 in
  let sum_cals n =
    Str.split (Str.regexp "\n\n") input
    |> List.map
         ~f:
           (compose sum
           @@ compose (List.map ~f:int_of_string) (String.split ~on:'\n'))
    |> compose List.rev @@ List.sort ~compare
    |> compose sum @@ flip List.take n
  in
  (string_of_int @@ sum_cals 1, string_of_int @@ sum_cals 3)
