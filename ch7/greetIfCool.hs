module GreetIfCool where
GreetIfCool coolness = 
  case cool of
    True ->
      putStrLn "eyyyy. What's shaking'?"
    False -> putStrLn "pshhhh."
  where cool == "downright frosty yo" 