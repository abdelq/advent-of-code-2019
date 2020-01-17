open Base
open Stdio

let part1 layers =
  let count_digit digit = List.count ~f:(phys_equal digit) in
  let compare = Comparable.lift compare ~f:(count_digit '0') in
  match List.min_elt layers ~compare with
  | Some l -> printf "%d\n" ((count_digit '1' l) * (count_digit '2' l))
  | _ -> assert false

let part2 layers width =
  let decode = function
    | '0' -> Some ' '
    | '1' -> Some 'X'
    | _ -> None in
  List.iteri (List.transpose_exn layers) ~f:(fun i pixels ->
      printf "%c" (List.find_map_exn pixels ~f:decode);
      if i % width = width - 1 then printf "\n")

let () =
  let width, length = 25, 6
  and image = In_channel.with_file "input" ~f:In_channel.input_line_exn in
  let layers = List.chunks_of (String.to_list image) ~length:(width * length) in
  part1 layers; part2 layers width
