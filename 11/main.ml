open Base
open Stdio

let rec paint (Robot.{ position; computer; _ } as robot) panels =
  match Intcode.run computer with
  | `Halt -> panels
  | `Input ->
    Intcode.add_input computer
      (match Map.find panels position with
       | Some color -> color
       | None -> 0);
    paint robot panels
  | `Output data -> (* color *)
    match Intcode.run computer with
    | `Output direction ->
      paint (Robot.move robot direction) (Map.set panels ~key:position ~data)
    | _ -> assert false

let part1 robot =
  let panels = paint robot (Map.empty (module Coordinate)) in
  printf "%d\n" (Map.length panels)

let part2 robot =
  let corner cmp = Map.fold ~init:(0, 0)
      ~f:(fun ~key:(a, b) ~data:_ (c, d) -> cmp a c, cmp b d) in
  let panels = paint robot (Map.singleton (module Coordinate) (0, 0) 1) in
  let (x0, y0), (x1, y1) = corner min panels, corner max panels in
  for y = y0 to y1 do
    for x = x0 to x1 do
      match Map.find panels (x, y) with
      | Some 1 -> printf "X"
      | _ -> printf " "
    done;
    printf "\n"
  done

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string) in
  let robot1, robot2 = Robot.init computer, Robot.init computer in
  part1 robot1; part2 robot2
