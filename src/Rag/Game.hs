module Rag.Game (startMaze) where
import Rag.Data
import Rag.Types
import Rag.Parser
import System.Console.Readline (readline)
import qualified Data.String.Utils as U (strip)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Control.Monad.Writer (tell)
import qualified Data.IntMap as Map


startMaze mazeDef = 
  let start = lookupRoom errorRoom 1 mazeDef 
  in mazeLoop (roomFields start) (start, mazeDef)

mazeLoop :: [String] -> GameState -> IO ()
mazeLoop messages state@(r,md) = do
  mapM_ putStrLn $ wrapLines messages 
  putStrLn ""
  maybeLine <- readline "> "
  case maybeLine of
    Nothing         -> return ()
    Just ":quit"    -> putStrLn "Thank you very much!"
    Just cmd  -> uncurry mazeLoop $ doCmd cmd state

doCmd :: String -> GameState -> ([String], GameState)   
doCmd "look"  state@(room,_) = ([describeRoom room], state)
doCmd command state@(room,_) = 
  case List.find ((command ==) . name) (outcomes room) of
    Just o -> runHandler (handleOutcome o) state
    Nothing -> (["I don't understand."], state)

handleOutcome :: Outcome -> Handler ()
handleOutcome (Action n r) = tell [r] 
handleOutcome (Edge _ dest _) = do
  maze <- getMaze
  let newRoom = lookupRoom errorRoom dest maze in
    do tell (roomFields newRoom)
       goto newRoom
       
errorRoom = Room { title = "The Void",
                   desc  = "You are in the void. The creator has erred deeply.",
                   outcomes = [(Edge "start" 1 False)]}
                        
wrapLines =  (>>= wrapToLines 80)
wrapToLines :: Int -> String -> [String]
wrapToLines n = wrap' n . words where
  lengths    = tail . scanl (\x s -> x + length s + 1) (-1)
  numToTake  = max 1 . length . takeWhile (<= n) . lengths
  wrap' _ [] = []
  wrap' n ss = let (b, e) = splitAt (numToTake ss) ss in unwords b : wrap' n e