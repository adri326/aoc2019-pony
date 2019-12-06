use "files"

primitive IO
  fun apply(path: String, env: Env, callback: {(String val): None}) =>
    try
      let file = File.open(FilePath(env.root as AmbientAuth, path)?)
      let source = file.read_string(file.size())
      callback(consume source)
    else
      env.out.print("Could not open file!")
    end
