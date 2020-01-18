open Base
open Stdio

exception Invalid_direction of char

let parse_path path =
  let trace ((x, y), steps) direction amount =
    let positions = List.init amount
        ~f:(match direction with
            | 'U' -> fun i -> (x, y + i + 1), steps + i + 1
            | 'D' -> fun i -> (x, y - i - 1), steps + i + 1
            | 'L' -> fun i -> (x - i - 1, y), steps + i + 1
            | 'R' -> fun i -> (x + i + 1, y), steps + i + 1
            | _ -> raise (Invalid_direction direction)) in
    List.last_exn positions, positions in
  List.folding_map path ~init:((0, 0), 0) ~f:(fun acc move ->
      trace acc move.[0] (Int.of_string (String.subo move ~pos:1)))
  |> List.concat
  |> Map.of_alist_reduce (module Coordinate) ~f:(fun a _ -> a)

let part1 wire1 wire2 =
  Map.fold2 wire1 wire2 ~init:Int.max_value ~f:(fun ~key:pos ~data acc ->
      match data with
      | `Both _ -> min acc (Coordinate.central_dist pos)
      | _ -> acc)
  |> printf "%d\n"

let part2 wire1 wire2 =
  Map.fold2 wire1 wire2 ~init:Int.max_value ~f:(fun ~key:_ ~data acc ->
      match data with
      | `Both (steps1, steps2) -> min acc (steps1 + steps2)
      | _ -> acc)
  |> printf "%d\n"

let () =
  let paths = In_channel.read_lines "input"
              |> List.map ~f:(String.split ~on:',') in
  let wire1 = parse_path (List.nth_exn paths 0)
  and wire2 = parse_path (List.nth_exn paths 1) in
  part1 wire1 wire2; part2 wire1 wire2
