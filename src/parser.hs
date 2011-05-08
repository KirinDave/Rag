module Rag.Parser (parseRagFile) where
import Rag.Data
import Rag.Types
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import qualified Data.IntMap as Map (insert, empty)

-- Parsing utilities
parseRagFile :: String -> IO MazeDefinition
parseRagFile fileName = do
  dat <- readFile fileName
  case parse ragFile fileName dat of
    Left why -> do print why ; return $ buildMaze []
    Right xs -> return $ buildMaze xs
    
buildMaze :: [(Int, Room)] -> MazeDefinition
buildMaze = foldr (uncurry Map.insert) Map.empty 


-- Parser

ragFile = do 
  results <- ragLine `sepEndBy` (many newline) 
  return results

ragLine :: GenParser Char st (Int, Room)
ragLine = do 
  id <- num ; char '|'
  title <- cellContent ; char '|'
  desc <- cellContent ; char '|'
  visibleEdges <- edges False ; char '|'
  hiddenEdges <- edges True ; char '|'
  verbs <- actions
  return  (id, Room title desc (visibleEdges ++ hiddenEdges ++verbs))

cellContent :: GenParser Char st String
cellContent = many (noneOf "|\n")

edges :: Bool -> GenParser Char st [Outcome]
edges isHidden = edgeDef isHidden `sepBy` char ','

edgeDef :: Bool -> GenParser Char st Outcome
edgeDef isHidden = do
  name <- many letter ; char '='
  loc <- num
  return $ Edge name loc isHidden

actions :: GenParser Char st [Outcome]
actions = actionDef `sepBy` char '\\'
                             
actionDef = do
  name   <- many (noneOf "=\n") ; char '='
  result <- many (noneOf "\\\n")
  return $ Action name result
                       
num :: GenParser Char st Int
num = liftM read (many digit)