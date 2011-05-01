module Rag.Main where
import Rag.Data
import Rag.Parser

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Data.Monoid

type MazeDefinition = Map.Map Int Room

parseFile :: String -> IO [(Int, Room)]
parseFile fileName = do
  dat <- readFile fileName
  case parse ragFile fileName dat of
    Left why -> do putStrLn $ show why
                   return []
    Right xs -> return xs

buildMaze :: String -> IO MazeDefinition
buildMaze fileName = do
  dat         <- readFile fileName
  pairs       <- parseFile dat
  return $ foldr (uncurry Map.insert) Map.empty pairs
  