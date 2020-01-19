open Base
open Stdio

let step ~orig:(x0, y0) ~dest:(x1, y1) =
  let rec gcd a b =
    if b = 0 then a else gcd b (a % b) in
  match x1 - x0, y0 - y1 with
  | 0, 0 -> 0, 0
  | dx, dy -> let norm = gcd (abs dx) (abs dy) in dx / norm, dy / norm

let count_detections asteroids (ax, ay as orig) =
  let rec can_detect (px, py as pos) (sx, sy as step) goal =
    Coordinate.equal pos goal ||
    not (Set.mem asteroids pos) &&
    can_detect (px + sx, py + sy) step goal in
  Set.count asteroids ~f:(fun dest ->
      match step ~orig ~dest with
      | 0, 0 -> false
      | dx, dy -> can_detect (ax + dx, ay + dy) (dx, dy) dest)

let monitoring_station asteroids =
  let asteroid = Set.choose_exn asteroids in
  let init = asteroid, count_detections asteroids asteroid in
  Set.fold asteroids ~init ~f:(fun (pos1, det1) pos2 ->
      let det2 = count_detections asteroids pos2 in
      if det1 > det2 then pos1, det1 else pos2, det2)

let part1 detections =
  printf "%d\n" detections

let () =
  let asteroids =
    In_channel.read_lines "input"
    |> List.foldi ~init:(Set.empty (module Coordinate)) ~f:(fun y coords ->
        String.foldi ~init:coords ~f:(fun x coords -> function
            | '#' -> Set.add coords (x, y)
            | _ -> coords)) in
  let _, detections = monitoring_station asteroids in
  part1 detections
