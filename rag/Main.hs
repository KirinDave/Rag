module Rag.Main where
import Rag.Data
import Rag.Types
import qualified Data.String.Utils as U (strip, join)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Writer 
import qualified Data.IntMap as Map
  
mazeLoop :: GameState -> IO GameState
mazeLoop state@(r,md) = do
  putStr "> "
  command <- U.strip `fmap` getLine ; putStrLn ""
  case command of
    ":quit"    -> do putStrLn "Thank you very much!"
                     return state
    otherwise  -> 
      let (state, messages) = doCmd command state in
      do mapM_ putStrLn messages 
         mazeLoop state

doCmd :: String -> GameState -> (GameState, [String])   
doCmd "look" s@(room,_) = (s, [describeRoom room])
doCmd command state@(room,_) = 
  case List.find ((command ==) . name) (outcomes room) of
    Just o -> execState (runWriterT (handleOutcome o)) state
    Nothing -> (state, ["I don't understand."])


getRoom :: Handler Room
getRoom = get >>= return . fst

getMaze :: Handler MazeDefinition
getMaze = do state <- get
             return (snd state)

goto room = do
  maze <- getMaze
  put (room,maze)

handleOutcome :: Outcome -> Handler ()
handleOutcome (Action n r) = 
  tell [r] 
handleOutcome (Edge _ dest _) = do
  maze <- getMaze
  let newRoom = Map.findWithDefault errorRoom dest maze in
    do tell [describeRoom newRoom]
       goto newRoom
  
errorRoom = Room { title = "The Void",
                   desc  = "You are in the void. The creator has erred deeply.",
                   outcomes = [(Edge "start" 1 False)]}
                        
