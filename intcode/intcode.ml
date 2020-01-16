open Base

exception Invalid_opcode of int
exception Invalid_mode of int

type computer = {
  mutable mem: int array;
  mutable instr_ptr: int;
  mutable rel_base: int;
  input: int Queue.t
}

let of_string code =
  let mem = String.split ~on:',' code |> Array.of_list_map ~f:Int.of_string in
  { mem; instr_ptr = 0; rel_base = 0; input = Queue.create () }

let copy { mem; instr_ptr; rel_base; input } =
  { mem = Array.copy mem; instr_ptr; rel_base; input = Queue.copy input }

let add_input computer = Queue.enqueue computer.input

let resize_mem computer min_index =
  let new_mem = Array.create 0 ~len:(Int.ceil_pow2 (min_index + 1)) in
  Array.blito ~src:computer.mem ~dst:new_mem ();
  computer.mem <- new_mem

(* TODO Verify total number of parameters *)
let instr_param ({ mem; instr_ptr; rel_base; _ } as comp) ?(write = false) pos =
  let param = match (mem.(instr_ptr) / 10 ** (pos + 1)) % 10 with
    | 0 -> mem.(instr_ptr + pos)               (* Position *)
    | 1 -> assert (not write); instr_ptr + pos (* Immediate *)
    | 2 -> mem.(instr_ptr + pos) + rel_base    (* Relative *)
    | m -> raise (Invalid_mode m) in
  if param >= Array.length mem then resize_mem comp param;
  param

let rec run ({ instr_ptr; rel_base; input; _ } as comp) =
  let param = instr_param comp in
  match Int.rem comp.mem.(instr_ptr) 100 with
  | 1 | 2 | 7 | 8 as opcode -> (* Add | Multiply | Less_than | Equals *)
    let src1, src2, dst = param 1, param 2, param 3 ~write:true in
    comp.mem.(dst) <- (match opcode with
        | 1 -> comp.mem.(src1) + comp.mem.(src2)
        | 2 -> comp.mem.(src1) * comp.mem.(src2)
        | 7 -> Bool.to_int (comp.mem.(src1) < comp.mem.(src2))
        | 8 -> Bool.to_int (comp.mem.(src1) = comp.mem.(src2))
        | _ -> raise (Invalid_opcode opcode));
    comp.instr_ptr <- instr_ptr + 4;
    run comp
  | 3 -> (* Input *)
    let dst = param 1 ~write:true in
    (match Queue.dequeue input with
     | None -> `Input
     | Some i ->
       comp.mem.(dst) <- i;
       comp.instr_ptr <- instr_ptr + 2;
       run comp)
  | 4 -> (* Output *)
    let src = param 1 in
    comp.instr_ptr <- instr_ptr + 2;
    `Output comp.mem.(src)
  | 5 | 6 as opcode -> (* Jump_if_true | Jump_if_false *)
    let src1, src2 = param 1, param 2
    and cmp = if opcode = 5 then (<>) else (=) in
    comp.instr_ptr <-
      if cmp comp.mem.(src1) 0 then
        comp.mem.(src2)
      else
        instr_ptr + 3;
    run comp
  | 9 -> (* Adjust_relative_base *)
    let src = param 1 in
    comp.rel_base <- rel_base + comp.mem.(src);
    comp.instr_ptr <- instr_ptr + 2;
    run comp
  | 99 -> `Halt
  | opcode -> raise (Invalid_opcode opcode)
