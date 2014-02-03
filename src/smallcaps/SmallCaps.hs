module SmallCaps where

import Data.Text            ( Text )
import Data.Attoparsec.Text ( parseOnly )

import Data.LaTeX           ( LaTeX, unlatex )
import Data.Config          ( Config )
import Text.TeXParser       ( tex )
import Text.TeXLaTeXParser  ( parse, latex )
import Text.DocumentParser  ( runDocument )

smallcaps :: Config -> Text -> Either String Text
smallcaps conf = fmap unlatex . (runDocument conf =<<) . fmap fst . parseLaTeX

parseLaTeX :: Text -> Either String (LaTeX, [Text])
parseLaTeX = fmap (parse latex) . parseOnly tex

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
