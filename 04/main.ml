open Base
open Stdio

let digits_of_uint =
  let rec aux acc n =
    if n < 10 then n :: acc else aux (n % 10 :: acc) (n / 10) in
  aux []

let count_valid ~grp_criterion =
  let valid_len pass = List.length pass = 6
  and valid_ord pass = List.is_sorted pass ~compare
  and valid_grp pass = List.exists ~f:grp_criterion
      (List.group pass ~break:(<>) |> List.map ~f:List.length) in
  Sequence.count ~f:(fun pass ->
      valid_len pass && valid_ord pass && valid_grp pass)

let part1 passwords =
  printf "%d\n" (count_valid passwords ~grp_criterion:(fun len -> len > 1))

let part2 passwords =
  printf "%d\n" (count_valid passwords ~grp_criterion:(fun len -> len = 2))

let () =
  let a, b =
    In_channel.with_file "input" ~f:(fun chan ->
        In_channel.input_line_exn chan |> String.lsplit2_exn ~on:'-') in
  let passwords =
    Sequence.range ~stop:`inclusive (Int.of_string a) (Int.of_string b)
    |> Sequence.map ~f:digits_of_uint in
  part1 passwords; part2 passwords
