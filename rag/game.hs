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
  in mazeLoop [(describeRoom start)] (start, mazeDef)

mazeLoop :: [String] -> GameState -> IO GameState
mazeLoop messages state@(r,md) = do
  mapM_ putStrLn messages
  maybeLine <- readline "> "
  -- command <- U.strip `fmap` getLine ; putStrLn ""
  case maybeLine of
    Nothing         -> do return state
    Just ":quit"    -> do putStrLn "Thank you very much!"
                          return state
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
    do tell [describeRoom newRoom]
       goto newRoom
       
errorRoom = Room { title = "The Void",
                   desc  = "You are in the void. The creator has erred deeply.",
                   outcomes = [(Edge "start" 1 False)]}
                        
