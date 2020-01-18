open Base

module T = struct
  type t = int * int

  let sexp_of_t (x, y) =
    Sexp.List [Int.sexp_of_t x; Int.sexp_of_t y]

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with
    | 0 -> compare y0 y1
    | c -> c

  let manhattan_dist (x0, y0) (x1, y1) =
    abs (x1 - x0) + abs (y1 - y0)

  let central_dist ?(center = 0, 0) =
    manhattan_dist center
end

include T
include Comparator.Make (T)
