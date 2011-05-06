module Rag.Data where
import qualified Data.List as List
import Control.Applicative
import qualified Data.IntMap as Map (IntMap, insert, empty, lookup, (!)) 

data Outcome = Action { name   :: String, 
                        output :: String}
             | Edge { name :: String,
                      destination :: Int,
                      hidden :: Bool} deriving (Show, Eq)
isVisible x = 
  case x of 
    Action _ _ -> False
    Edge _ _ h -> h
    

data Room = Room { title   :: String,
                   desc    :: String,
                   exits   :: [Outcome],
                   actions :: [Outcome]} deriving (Show, Eq)

type MazeDefinition = Map.IntMap Room


findOutcome :: String -> Room -> Maybe Outcome
findOutcome command room = 
  List.find ((command ==) . name) (outcomes room)

resultOf :: Maybe Outcome -> GameState -> (String, GameState)
resultOf outcome state@(_,maze) = 
  case outcome of
    Nothing                 -> ("I don't understand.", state)
    Just (Action _ out)     -> (out, state)
    Just (Edge name dest _) -> (roomDesc (maze `getRoom` dest), 
                                state `withRoom` dest)

withRoom :: GameState -> Int -> GameState
withRoom (_, def) i = (i, def)

getRoom :: MazeDefinition -> Int -> Maybe Room
getRoom maze i = i `Map.lookup` maze <|> 1 `Map.lookup` maze

roomDesc :: Maybe Room -> String
roomDesc r = 
  case r of
    Just room@(Room t d _) -> t ++ "\n" ++ d ++ exitList room
    Nothing           -> "The endless void.\nYou have fallen off the map." ++
                         "Exits: None"

exitList = ("Exits: " ++) . buildCsl . filter isVisible . outcomes
           where buildCsl = List.intercalate ", " . map name 
