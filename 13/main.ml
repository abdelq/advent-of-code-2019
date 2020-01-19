open Stdio

let part1 computer =
  let rec count_blocks computer =
    let _, _ = Intcode.run computer, Intcode.run computer in
    match Intcode.run computer with
    | `Halt -> 0
    | `Output tile -> Bool.to_int (tile = 2) + count_blocks computer
    | _ -> assert false in
  let computer = Intcode.copy computer in
  printf "%d\n" (count_blocks computer)

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string) in
  part1 computer
