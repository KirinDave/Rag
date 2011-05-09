module Rag.Types where
import Rag.Data
import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap (IntMap, findWithDefault)

type GameState      = (Room, MazeDefinition)
type MazeDefinition = IntMap Room

lookupRoom = findWithDefault
  
-- Handler is our game loop abstraction
type Handler = WriterT [String] (State GameState)

runHandler th state = runState (execWriterT th) state

getRoom :: Handler Room
getRoom = do state <- get
             return (fst state)

getMaze :: Handler MazeDefinition
getMaze = do state <- get
             return (snd state)

goto room = do maze <- getMaze
               put (room,maze)
