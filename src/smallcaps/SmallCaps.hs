module SmallCaps where

import Data.Text            ( Text )
import Data.Attoparsec.Text ( parseOnly )
import Text.Parsec          ( parse )

import Data.LaTeX           ( unlatex )
import Data.Config          ( Config )
import Text.TeXParser       ( tex )
import Text.TeXLaTeXParser  ( latex )
import Text.DocumentParser  ( runDocument )

smallcaps :: Config -> Text -> Either String Text
smallcaps conf = fmap unlatex . parseDoc . parseLaTeX . parseTeX where
  parseTeX    = parseOnly tex
  parseLaTeX  = either Left (either (Left . show) Right . parse latex "")
  parseDoc    = either Left (runDocument conf)

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
