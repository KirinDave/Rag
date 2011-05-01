module Data.Rag where

import Data.List
import Data.Map

data Outcome = Action { name   :: String, 
                        output :: String}
             | Edge { name :: String,
                      destination :: Int,
                      hidden :: Bool} deriving (Show, Eq)

data Room = Room { title :: String,
                   desc  :: String,
                   outcomes :: [Outcome]} deriving (Show, Eq)

