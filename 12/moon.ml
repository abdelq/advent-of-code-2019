open Base
open Re

type coordinate = { x: int; y: int; z: int }
type moon = { pos: coordinate; vel: coordinate }

let of_string str =
  let pattern = [ Re.str "<x="; Re.str ", y="; Re.str ", z="; Re.str ">" ]
  and sep = group (seq [ opt (Re.char '-'); rep1 digit ]) in
  let group = exec (compile (seq (List.intersperse ~sep pattern))) str in
  let x = Int.of_string (Group.get group 1)
  and y = Int.of_string (Group.get group 2)
  and z = Int.of_string (Group.get group 3) in
  { pos = { x; y; z }; vel = { x = 0; y = 0; z = 0 } }

let apply_gravity moon =
  List.fold ~init:moon ~f:(fun { pos; vel } m ->
      { pos; vel = { x = vel.x + compare m.pos.x pos.x;
                     y = vel.y + compare m.pos.y pos.y;
                     z = vel.z + compare m.pos.z pos.z; } })

let apply_velocity { pos; vel } =
  { vel; pos = { x = pos.x + vel.x; y = pos.y + vel.y; z = pos.z + vel.z }}

let energy moon =
  let potential { pos; _ } = abs pos.x + abs pos.y + abs pos.z
  and kinetic { vel; _ } = abs vel.x + abs vel.y + abs vel.z in
  potential moon * kinetic moon
