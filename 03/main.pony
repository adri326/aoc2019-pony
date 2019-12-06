use "../io"
use "collections"
use "debug"

actor Main
  let env: Env
  new create(env': Env) =>
    env = env'
    IO("03/input.txt", env, this~next())

  fun next(source: String) =>
    let lines = recover val source.split_by("\n") end
    try
      let red = recover val lines(0)?.split_by(",") end
      let blue = recover val lines(1)?.split_by(",") end
      let parsed_red = recover val
        let red': Array[(Direction, ISize)] trn = recover trn Array[(Direction, ISize)](red.size()) end
        for term in red.values() do
          (let a, let b) = term.clone().chop(1)
          red'.push((
            match consume a
            | "D" => Down
            | "U" => Up
            | "R" => Right
            | "L" => Left
            else error
            end,
            b.isize()?
          ))
        end
        red'
      end

      let parsed_blue = recover val
        let blue': Array[(Direction, ISize)] trn = recover trn Array[(Direction, ISize)](blue.size()) end
        for term in blue.values() do
          (let a, let b) = term.clone().chop(1)
          blue'.push((
            match consume a
            | "D" => Down
            | "U" => Up
            | "R" => Right
            | "L" => Left
            else error
            end,
            b.isize()?
          ))
        end
        blue'
      end

      Solution(env, parsed_red, parsed_blue)
    else env.out.print("Invalid input") end

actor Solution
  let intersections: Array[(Coords val, USize)] = Array[(Coords val, USize)]
  let env: Env

  new create(env': Env, parsed_red: Array[(Direction, ISize)] val, parsed_blue: Array[(Direction, ISize)] val) =>
    env = env'
    let lines_red = recover ref Array[Line](parsed_red.size()) end
    let lines_blue = recover ref Array[Line](parsed_blue.size()) end

    var origin = recover ref Coords(0, 0) end
    var dist = USize(0)
    for line in parsed_red.values() do
      lines_red.push(line._1(line._2, origin, dist = dist + line._2.usize()))
    end

    origin = recover ref Coords(0, 0) end
    dist = 0
    for line in parsed_blue.values() do
      let line' = line._1(line._2, origin, dist = dist + line._2.usize())
      lines_blue.push(line')
      for candidate in lines_red.values() do
        if candidate.vertical != line'.vertical then
          if line'.vertical then
            if (candidate.from.x.min(candidate.to.x) <= line'.from.x)
              and (candidate.from.x.max(candidate.to.x) >= line'.from.x)
              and (candidate.from.y <= line'.from.y.max(line'.to.y))
              and (candidate.from.y >= line'.from.y.min(line'.to.y))
            then
              intersect(Coords(line'.from.x, candidate.from.y), candidate, line')
            end
          else
            if (candidate.from.y.min(candidate.to.y) <= line'.from.y)
              and (candidate.from.y.max(candidate.to.y) >= line'.from.y)
              and (candidate.from.x <= line'.from.x.max(line'.to.x))
              and (candidate.from.x >= line'.from.x.min(line'.to.x))
            then
              intersect(Coords(candidate.from.x, line'.from.y), candidate, line')
            end
          end
        end
      end
    end

    next()

  fun ref intersect(coords: Coords val, red: Line, blue: Line) =>
    intersections.push((coords, red.distance(coords) + blue.distance(coords)))

  fun next() =>
    var smallest_distance: USize = -1
    var smallest_length: USize = -1
    for intersection in intersections.values() do
      let distance = intersection._1.x.abs().usize() + intersection._1.y.abs().usize()
      if distance < smallest_distance then smallest_distance = distance end
      if intersection._2 < smallest_length then smallest_length = intersection._2 end
    end
    env.out.print(smallest_distance.string())
    env.out.print(smallest_length.string())

primitive Up
  fun apply(n: ISize, origin: Coords ref, dist: USize): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x, origin.y + n), true, dist)
    origin.y = origin.y + n
    line

primitive Down
  fun apply(n: ISize, origin: Coords ref, dist: USize): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x, origin.y - n), true, dist)
    origin.y = origin.y - n
    line

primitive Left
  fun apply(n: ISize, origin: Coords ref, dist: USize): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x - n, origin.y), false, dist)
    origin.x = origin.x - n
    line

primitive Right
  fun apply(n: ISize, origin: Coords ref, dist: USize): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x + n, origin.y), false, dist)
    origin.x = origin.x + n
    line

type Direction is (Up | Down | Left | Right)

class Coords
  var x: ISize
  var y: ISize

  new iso create(x': ISize, y': ISize) =>
    x = x'
    y = y'
  
  fun box clone(): Coords iso =>
    Coords(this.x, this.y)

class Line
  let from: Coords val
  let to: Coords val
  let vertical: Bool
  let _dist: USize

  new create(from': Coords val, to': Coords val, vertical': Bool, dist': USize) =>
    from = from'
    to = to'
    vertical = vertical'
    _dist = dist'
  
  fun distance(coords: Coords val): USize =>
    _dist + (coords.x - from.x).abs().usize() + (coords.y - from.y).abs().usize()

primitive Red
primitive Blue
type Color is (Red | Blue | None)
