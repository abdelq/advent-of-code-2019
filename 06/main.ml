open Base
open Stdio

let part1 orbits =
  let rec count_orbits obj depth =
    List.sum (module Int) (Map.find_multi orbits obj) ~f:(fun orbiter ->
        depth + count_orbits orbiter (depth + 1)) in
  printf "%d\n" (count_orbits "COM" 1)

let part2 orbits =
  let rec path_to_center obj =
    match Map.find orbits obj with
    | None -> [obj]
    | Some orbited -> obj :: (path_to_center orbited) in
  let from_you, from_san = path_to_center "YOU", path_to_center "SAN" in
  let transfers =
    List.find_mapi_exn from_you ~f:(fun i obj1 ->
        List.find_mapi from_san ~f:(fun j obj2 ->
            if String.equal obj1 obj2 then Some (i + j - 2) else None)) in
  printf "%d\n" transfers

let () =
  let map =
    In_channel.read_lines "input" |> List.map ~f:(String.lsplit2_exn ~on:')') in
  let orbits1 =
    Map.of_alist_multi (module String) map
  and orbits2 =
    List.map map ~f:(fun (a, b) -> b, a) |> Map.of_alist_exn (module String) in
  part1 orbits1; part2 orbits2
