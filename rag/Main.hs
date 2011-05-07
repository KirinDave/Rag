module Rag.Main where
import Rag.Data
import Rag.Types
import Rag.Parser
import qualified Data.String.Utils as U (strip)
import qualified Data.List as List
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Control.Monad.Writer (tell)
import qualified Data.IntMap as Map

main = do
  args     <- getArgs
  let fName = head args in
    do putStrLn $ "Welcome to Rag. Loading file: " ++ fName
       mazeDef <- parseRagFile fName
       startMaze mazeDef
  
startMaze mazeDef = 
  let start = lookupRoom errorRoom 1 mazeDef 
  in mazeLoop [(describeRoom start)] (start, mazeDef)

mazeLoop :: [String] -> GameState -> IO GameState
mazeLoop messages state@(r,md) = do
  mapM_ putStrLn messages
  putStr "> "
  command <- U.strip `fmap` getLine ; putStrLn ""
  case command of
    ":quit"    -> do putStrLn "Thank you very much!"
                     return state
    otherwise  -> uncurry mazeLoop $ doCmd command state

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
                        
