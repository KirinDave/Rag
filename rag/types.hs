module Rag.Types (Handler, GameState, MazeDefinition) where
import Rag.Data
import Control.Monad.State
import Control.Monad.Writer
import Data.IntMap (IntMap) 

type GameState      = (Room, MazeDefinition)
type Handler = WriterT [String] (State GameState)
type MazeDefinition = IntMap Room
