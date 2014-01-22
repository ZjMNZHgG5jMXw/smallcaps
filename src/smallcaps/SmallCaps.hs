module SmallCaps where

import Data.Text            ( Text )
import Data.Attoparsec.Text ( parseOnly )
import Text.Parsec          ( parse )

import Data.LaTeX           ( unlatex )
import Data.Config          ( Config )
import Text.TeXParser       ( tex )
import Text.TeXLaTeXParser  ( latex )
import Text.DocumentParser  ( runDocument )

smallcaps :: Config -> Text -> Text
smallcaps conf = unlatex . runDocument conf . parseLaTeX . parseTeX where
  parseTeX = either error id . parseOnly tex
  parseLaTeX = either (error . show) id . parse latex ""

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
