module Rag.Types (Handler, GameState, MazeDefinition) where
import Rag.Data
import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap (IntMap) 

type GameState      = (Room, MazeDefinition)
type MazeDefinition = IntMap Room

type Handler = WriterT [String] (State GameState)

runHandler th state = runState (execWriterT th) state
