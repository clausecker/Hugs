--
-- Calling a static method
--
module Env where

import DotNet

foreign import dotnet
  "static System.Environment.GetEnvironmentVariable"
  getEnv :: String -> IO String

test :: IO ()
test = do
  let var = "COMSPEC"
  s <- getEnv var
  putStrLn (var ++ " = " ++ s)
