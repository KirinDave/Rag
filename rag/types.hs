module Rag.Types (Handler, GameState, MazeDefinition) where
import Rag.Data
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IntMap (IntMap) 

type GameState      = (Room, MazeDefinition)
type Handler = WriterT [String] (Reader GameState)
type MazeDefinition = IntMap Room
newtype MetaAction = C { runMeta :: GameState -> Handler GameState }