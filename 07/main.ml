open Base
open Stdio

let rec permutations lst =
  let rec insert x = function
    | [] -> [[x]]
    | hd :: tl -> (x :: hd :: tl) :: List.map (insert x tl) ~f:(List.cons hd) in
  match lst with
  | hd :: tl -> List.concat_map (permutations tl) ~f:(insert hd)
  | _ -> [lst]

let amplifiers computer =
  List.map ~f:(fun phase ->
      let amplifier = Intcode.copy computer in
      Intcode.add_input amplifier phase;
      amplifier)

let part1 computer sequences =
  let amplify sequence =
    List.fold (amplifiers computer sequence) ~init:0 ~f:(fun signal amp ->
        Intcode.add_input amp signal;
        match Intcode.run amp with
        | `Output signal -> signal
        | _ -> assert false) in
  printf "%d\n" (List.fold (List.map sequences ~f:amplify) ~init:0 ~f:max)

let part2 computer sequences =
  let amplify sequence =
    let rec loop amplifiers input =
      let output = List.fold amplifiers ~init:input ~f:(fun signal amp ->
          Intcode.add_input amp signal;
          match Intcode.run amp with
          | `Halt -> -1
          | `Output signal -> signal
          | _ -> assert false) in
      if output = -1 then input else loop amplifiers output in
    loop (amplifiers computer sequence) 0 in
  printf "%d\n" (List.fold (List.map sequences ~f:amplify) ~init:0 ~f:max)

let () =
  let computer = In_channel.with_file "input" ~f:(fun chan ->
      In_channel.input_line_exn chan |> Intcode.of_string)
  and sequences1 = permutations [0; 1; 2; 3; 4]
  and sequences2 = permutations [5; 6; 7; 8; 9] in
  part1 computer sequences1; part2 computer sequences2
