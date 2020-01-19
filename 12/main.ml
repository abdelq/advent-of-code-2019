open Base
open Stdio

let part1 moons =
  let rec simulate steps moons =
    let time_step moon =
      Moon.apply_velocity (Moon.apply_gravity moon moons) in
    if steps = 0 then
      moons
    else
      simulate (steps - 1) (List.map moons ~f:time_step) in
  printf "%d\n" (List.sum (module Int) ~f:Moon.energy (simulate 1000 moons))

let () =
  let moons = In_channel.read_lines "input" |> List.map ~f:Moon.of_string in
  part1 moons
