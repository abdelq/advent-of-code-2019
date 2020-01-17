open Stdio

let part1 computer =
  let computer = Intcode.copy computer in
  Intcode.add_input computer 1;
  match Intcode.run computer with
  | `Output keycode -> printf "%d\n" keycode
  | _ -> assert false

let part2 computer =
  let computer = Intcode.copy computer in
  Intcode.add_input computer 2;
  match Intcode.run computer with
  | `Output keycode -> printf "%d\n" keycode
  | _ -> assert false

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string) in
  part1 computer; part2 computer
