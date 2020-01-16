open Base
open Stdio

let part1 masses =
  let fuel mass = mass / 3 - 2 in
  printf "%d\n" (List.sum (module Int) masses ~f:fuel)

let part2 masses =
  let rec fuel mass =
    let f = mass / 3 - 2 in
    if f <= 0 then 0 else f + fuel f in
  printf "%d\n" (List.sum (module Int) masses ~f:fuel)

let () =
  let masses = In_channel.read_lines "input" |> List.map ~f:Int.of_string in
  part1 masses; part2 masses
