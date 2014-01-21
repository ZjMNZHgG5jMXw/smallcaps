module SmallCaps where

import Prelude hiding ( getContents, putStr )

import Data.Text            ( Text )
import Data.Text.IO         ( getContents, putStr )
import Data.Attoparsec.Text ( parseOnly )
import Text.Parsec          ( parse )

import Data.LaTeX
import Data.Config
import Text.TeXParser
import Text.TeXLaTeXParser
import Text.DocumentParser

smallcaps :: Config -> Text -> Text
smallcaps conf = unlatex . runDocument conf . parseLaTeX . parseTeX where
  parseTeX = either error id . parseOnly tex
  parseLaTeX = either (error . show) id . parse latex ""

smallcapsPipe :: Config -> IO ()
smallcapsPipe conf = putStr . smallcaps conf =<< getContents

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
