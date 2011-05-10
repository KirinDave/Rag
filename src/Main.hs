module Main where
import Rag.Game
import Rag.Parser
import System.Environment (getArgs)

main = getArgs >>= startGame

startGame []      = do putStrLn "No gamefile provded."
startGame [fName] = do 
  putStrLn $ "Welcome to Rag. Loading file: " ++ fName
  mazeDef <- parseRagFile fName
  startMaze mazeDef