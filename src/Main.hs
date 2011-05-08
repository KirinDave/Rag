module Main where
import Rag.Game
import Rag.Parser
import System.Environment (getArgs)

main = do
  args     <- getArgs
  let fName = head args in
    do putStrLn $ "Welcome to Rag. Loading file: " ++ fName
       mazeDef <- parseRagFile fName
       startMaze mazeDef
