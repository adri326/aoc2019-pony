use "ponytest"
use "debug"
use "collections"
use "../io"

actor Main is TestList
  let env: Env
  new create(env': Env) =>
    env = env'
    try
      if env.args(1)? == "test" then
        PonyTest(env, this)
      end
    else None end
    IO("02/input.txt", env, this~next())

  fun next(source: String) =>
    try
      let raw = Parser(source, true)
      env.out.print(Solution(raw.clone())()?.string()) // question 01
      for i in Range[USize](0, 100) do
        for j in Range[USize](0, 100) do
          raw(1)? = i
          raw(2)? = j
          if Solution(raw.clone())()? == 19690720 then
            env.out.print(((i * 100) + j).string())
          end
        end
      end
    else env.out.print("Error executing program!") end

  // new make() => None
  be tests(test: PonyTest) =>
    test(_Test1)

class _Test1 is UnitTest
  fun name(): String => "Basic Test"
  fun apply(h: TestHelper) =>
    try
      h.assert_eq[USize](Solution([1; 0; 0; 0; 99])()?, 2)
    else
      None
    end

class Solution
  let array: Array[USize] ref
  new create(array': Array[USize] ref) =>
    array = array'

  fun ref add(a: USize, b: USize, c: USize)? =>
    array(c)? = array(a)? + array(b)?

  fun ref mul(a: USize, b: USize, c: USize)? =>
    array(c)? = array(a)? * array(b)?

  fun ref apply(): USize? =>
    var pointer = USize(0)
    while array(pointer)? != 99 do
      if array(pointer)? == 1 then
        add(array(pointer + 1)?, array(pointer + 2)?, array(pointer + 3)?)?
      elseif array(pointer)? == 2 then
        mul(array(pointer + 1)?, array(pointer + 2)?, array(pointer + 3)?)?
      end
      pointer = pointer + 4
    end
    array(0)?

primitive Parser
  fun apply(input: String, replace: Bool = true): Array[USize] ref =>
    let s = recover box input.split_by(",") end
    let array: Array[USize] ref = Array[USize](s.size())
    for i in s.values() do
      try
        array.push(i.usize()?)
      else
        Debug("Invalid value: " + i)
      end
    end
    if replace then
      try
        array(1)? = 12
        array(2)? = 2
      else 
        Debug("Couldn't write 1202 to array!")
      end
    end
    array
