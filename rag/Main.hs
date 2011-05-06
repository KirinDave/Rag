module Rag.Main where
import Rag.Data
import Data.String.Utils (strip)
import qualified Data.List as List
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Data.IntMap as Map (IntMap, insert, empty, lookup, (!)) 

type GameState      = (Int, MazeDefinition)

playMaze :: GameState -> IO GameState
playMaze gs@(room, maze) = do
  putStr "> " 
  command <- strip `fmap` getLine
  let currentRoom         = maze `getRoom` room
      anOutcome           = currentRoom >>= findOutcome command
      (messag, newState) = resultOf anOutcome gs in
    do putStrLn message
       playMaze newState
       

  
mazeLoop :: GameState -> IO GameState
mazeLoop state@(room, maze) = do
  putStr "> "
  command <- strip `fmap` getLine