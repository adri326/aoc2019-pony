use "../io"

actor Main
  let env: Env
  new create(env': Env) =>
    env = env'
    IO("01/input.txt", env, this~calc_mass())

  fun calc_mass(bits: Array[String val] val) =>
    var sum = I32(0)
    var sum' = I32(0)
    for bit in bits.values() do
      if bit == "" then continue end
      try
        var module_fuel = ((bit.i32()? / 3) - 2)
        sum = sum + module_fuel

        while module_fuel > 0 do
          sum' = sum' + module_fuel
          module_fuel = (module_fuel / 3) - 2
        end
      else
        env.out.print("Invalid number: " + bit)
      end
    end

    env.out.print(sum.string())
    env.out.print(sum'.string())
