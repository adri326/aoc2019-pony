use "files"

primitive IO
  fun apply(path: String, env: Env, callback: {(Array[String val] val): None}) =>
    try
      let file = File.open(FilePath(env.root as AmbientAuth, path)?)
      let source = recover box file.read_string(file.size()) end
      let lines = source.split_by("\n")
      callback(consume lines)
    else
      env.out.print("Could not open file!")
    end
