module Data.StopState where

import Data.Default

type SubParser a = StopState -> a -> (a, StopState)

data StopState
  = None
  | NewLine
  | Stop
  | NewSentence
  | Skip
  deriving (Show, Eq)

instance Default StopState where
  def = NewSentence

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
