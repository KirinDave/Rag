module Rag.Main where
import Rag.Data
import Rag.Types
import qualified Data.String.Utils as U (strip, join)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer 
import qualified Data.IntMap as Map (IntMap, insert, empty, lookup, (!)) 
  
mazeLoop :: GameState -> IO GameState
mazeLoop state@(r,md) = do
  putStr "> "
  command <- U.strip `fmap` getLine ; putStrLn ""
  case command of
    "look"  -> do putStrLn $ describeRoom r
                  mazeLoop state
    ":quit" -> do putStrLn "Thank you very much!"
                  return state
    _       -> 
      let (newRoom, messages) = doCmd command state in
      do mapM_ putStrLn messages 
         mazeLoop (newRoom,md)

doCmd :: String -> GameState -> (Room, [String])    
doCmd command state@(room,_) = 
    runReader (runWriterT (handleOutcome o)) state 
    where fn = List.find ((command ==) . name) 
          o = fn (outcomes room) 

getRoom :: Handler Room
getRoom = ask >>= return . fst

getMaze :: Handler MazeDefinition
getMaze = do state <- ask
             return (snd state)
stayPut = getRoom

handleOutcome :: Maybe Outcome -> Handler Room
handleOutcome Nothing = do
  tell ["I don't understand!"]
  stayPut  

handleOutcome (Just (Action n r)) = 
  tell [r] >> stayPut

handleOutcome (Just (Edge _ dest _)) = do
  maze <- getMaze
  let newRoom = fromMaybe defaultRoomDefinition 
                          (Map.lookup dest maze) in do
    tell [describeRoom newRoom]
    return newRoom
  
defaultRoomDefinition = Room { title = "The Void",
                               desc  = "You are in the void. The creator has erred deeply.",
                               outcomes = [(Edge "start" 1 False)]}
                        
