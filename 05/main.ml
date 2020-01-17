open Stdio

let rec diagnostic ?(code = 0) computer =
  match Intcode.run computer with
  | `Halt -> code
  | `Output code -> diagnostic computer ~code
  | _ -> assert false

let part1 computer =
  let computer = Intcode.copy computer in
  Intcode.add_input computer 1;
  printf "%d\n" (diagnostic computer)

let part2 computer =
  let computer = Intcode.copy computer in
  Intcode.add_input computer 5;
  printf "%d\n" (diagnostic computer)

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string) in
  part1 computer; part2 computer
