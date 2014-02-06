module SmallCaps.LaTeX where

import Data.Text ( Text, empty, singleton, pack, intercalate )

type LaTeX = [LaTeXElement]

data LaTeXElement
  = Printable Text          -- ^ (hopefully) printable text
  | Macro Text LaTeX        -- ^ macro name + following blocks
  | Environment Text LaTeX  -- ^ environment name + content
  | Block LaTeX             -- ^ separate block
  | Comment Text            -- ^ comment starting with '%'
  deriving (Eq, Show)

isPrintable :: LaTeXElement -> Bool
isPrintable (Printable _) = True
isPrintable _             = False

isMacro :: LaTeXElement -> Bool
isMacro (Macro _ _) = True
isMacro _           = False

isEnvironment :: LaTeXElement -> Bool
isEnvironment (Environment _ _) = True
isEnvironment _                 = False

isBlock :: LaTeXElement -> Bool
isBlock (Block _) = True
isBlock _         = False

isComment :: LaTeXElement -> Bool
isComment (Comment _) = True
isComment _           = False

name :: LaTeXElement -> Text
name (Macro n _)        = n
name (Environment n _)  = n
name _                  = empty

content :: LaTeXElement -> Text
content (Printable text)  = text
content (Comment text)    = text
content _                 = empty

printable :: LaTeXElement -> Text
printable (Printable text)  = text
printable (Macro _ _)       = empty
printable x                 = cc $ map printable $ body x

body :: LaTeXElement -> LaTeX
body (Macro _ latex)        = latex
body (Environment _ latex)  = latex
body (Block latex)          = latex
body _                      = []

cc :: [Text] -> Text
cc = intercalate empty

unlatex :: LaTeX -> Text
unlatex = cc . map unlatexElement

unlatexElement :: LaTeXElement -> Text
unlatexElement (Printable text) = text
unlatexElement (Macro name' latex) = cc [name', unlatex latex]
unlatexElement (Environment name' latex) = cc
  [ pack "\\begin{", name', singleton '}'
  , unlatex latex
  , pack "\\end{", name', singleton '}'
  ]
unlatexElement (Block latex) = cc
  [ singleton '{'
  , unlatex latex
  , singleton '}'
  ]
unlatexElement (Comment text) = text

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
