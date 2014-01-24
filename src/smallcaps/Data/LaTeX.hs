module Data.LaTeX where

import Data.Text ( Text, singleton, pack, intercalate )

type LaTeX = [LaTeXElement]

data LaTeXElement
  = Printable Text          -- ^ (hopefully) printable text
  | Macro Text LaTeX        -- ^ macro name + following blocks
  | Environment Text LaTeX  -- ^ environment name + content
  | Block LaTeX             -- ^ separate block
  | Comment Text            -- ^ comment starting with '%'
  deriving Show

cc :: [Text] -> Text
cc = intercalate (pack "")

unlatex :: LaTeX -> Text
unlatex = cc . map unlatexElement

unlatexElement :: LaTeXElement -> Text
unlatexElement (Printable text) = text
unlatexElement (Macro name latex) = cc [name, unlatex latex]
unlatexElement (Environment name latex) = cc
  [ pack "\\begin{", name, singleton '}'
  , unlatex latex
  , pack "\\end{", name, singleton '}'
  ]
unlatexElement (Block latex) = cc
  [ singleton '{'
  , unlatex latex
  , singleton '}'
  ]
unlatexElement (Comment text) = text

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
