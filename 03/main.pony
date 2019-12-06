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
  let intersections: Array[Coords val] = Array[Coords val]
  let env: Env

  new create(env': Env, parsed_red: Array[(Direction, ISize)] val, parsed_blue: Array[(Direction, ISize)] val) =>
    env = env'
    let lines_red = recover ref Array[Line](parsed_red.size()) end
    var origin = recover ref Coords(0, 0) end
    for line in parsed_red.values() do
      lines_red.push(line._1(line._2, origin))
    end

    origin = recover ref Coords(0, 0) end
    for line in parsed_blue.values() do
      let line' = line._1(line._2, origin)
      for candidate in lines_red.values() do
        if candidate.vertical != line'.vertical then
          if line'.vertical then
            if (candidate.from.x.min(candidate.to.x) <= line'.from.x)
              and (candidate.from.x.max(candidate.to.x) >= line'.from.x)
              and (candidate.from.y <= line'.from.y.max(line'.to.y))
              and (candidate.from.y >= line'.from.y.min(line'.to.y))
            then
              intersect(Coords(line'.from.x, candidate.from.y))
            end
          else
            if (candidate.from.y.min(candidate.to.y) <= line'.from.y)
              and (candidate.from.y.max(candidate.to.y) >= line'.from.y)
              and (candidate.from.x <= line'.from.x.max(line'.to.x))
              and (candidate.from.x >= line'.from.x.min(line'.to.x))
            then
              intersect(Coords(candidate.from.x, line'.from.y))
            end
          end
        end
      end
    end

    next()

  be intersect(coords: Coords val) =>
    intersections.push(coords)

  be next() =>
    var smallest_distance: USize = -1
    for intersection in intersections.values() do
      let distance = intersection.x.abs().usize() + intersection.y.abs().usize()
      if distance < smallest_distance then smallest_distance = distance end
    end
    env.out.print(smallest_distance.string())

primitive Up
  fun apply(n: ISize, origin: Coords ref): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x, origin.y + n), true)
    origin.y = origin.y + n
    line

primitive Down
  fun apply(n: ISize, origin: Coords ref): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x, origin.y - n), true)
    origin.y = origin.y - n
    line

primitive Left
  fun apply(n: ISize, origin: Coords ref): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x - n, origin.y), false)
    origin.x = origin.x - n
    line

primitive Right
  fun apply(n: ISize, origin: Coords ref): Line =>
    let line = Line(Coords(origin.x, origin.y), Coords(origin.x + n, origin.y), false)
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

  new create(from': Coords val, to': Coords val, vertical': Bool) =>
    from = from'
    to = to'
    vertical = vertical'

primitive Red
primitive Blue
type Color is (Red | Blue | None)
