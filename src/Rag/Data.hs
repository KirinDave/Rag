module Rag.Data where
import Data.String.Utils (join)

-- Outcomes are text-addressible actions that are
-- available in rooms. 
data Outcome = Action { name   :: String, 
                        output :: String}
             | Edge { name :: String,
                      destination :: Int,
                      hidden :: Bool} deriving (Show, Eq)

isVisible (Edge _ _ h) = not h
isVisible _ = False
isHidden = not . isVisible

-- Rooms are the central abstraction of Rag, the user's commands
-- are checked against rooms
data Room = Room { title    :: String,
                   desc     :: String,
                   outcomes :: [Outcome]} deriving (Show, Eq)

exits = filter isVisible . outcomes
describeRoom r = 
  title r ++ "\n" ++
  desc  r ++ "\n" ++
  "Exits: " ++ (join ", " . map name $ exits r)