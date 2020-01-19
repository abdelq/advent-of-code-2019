type direction = North | East | South | West

type robot = {
  position: int * int;
  direction: direction;
  computer: Intcode.computer
}

let init computer =
  { position = 0, 0; direction = North; computer = Intcode.copy computer }

let move robot rotation =
  let direction =
    match robot.direction with
    | North -> if rotation = 0 then West else East
    | East -> if rotation = 0 then North else South
    | South -> if rotation = 0 then East else West
    | West -> if rotation = 0 then South else North in
  let position =
    let x, y = robot.position in
    match direction with
    | North -> x, y - 1
    | East -> x + 1, y
    | South -> x, y + 1
    | West -> x - 1, y in
  { robot with position; direction }
