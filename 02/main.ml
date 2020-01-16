open Stdio

let part1 computer =
  let ({ Intcode.mem; _ } as computer) = Intcode.copy computer in
  mem.(1) <- 12; mem.(2) <- 2;
  match Intcode.run computer with
  | `Halt -> printf "%d\n" mem.(0)
  | _ -> assert false

let part2 computer =
  for noun = 0 to 99 do
    for verb = 0 to 99 do
      let ({ Intcode.mem; _ } as computer) = Intcode.copy computer in
      mem.(1) <- noun; mem.(2) <- verb;
      match Intcode.run computer with
      | `Halt -> if mem.(0) = 19690720 then printf "%d\n" (100 * noun + verb)
      | _ -> assert false
    done
  done

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string) in
  part1 computer; part2 computer
