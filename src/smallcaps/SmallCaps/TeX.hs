module SmallCaps.TeX where

import Data.Text ( Text, empty, intercalate )
import Data.Char ( isPrint, isNumber, isSpace, isLetter )

type TeX = [TeXElement]

data TeXElement
  = Printable Text
  | Macro Text
  | Block TeX
  | Comment Text
  deriving (Eq, Show)

isPrintable :: TeXElement -> Bool
isPrintable (Printable _) = True
isPrintable _             = False

isMacro :: TeXElement -> Bool
isMacro (Macro _) = True
isMacro _         = False

isBlock :: TeXElement -> Bool
isBlock (Block _) = True
isBlock _         = False

isComment :: TeXElement -> Bool
isComment (Comment _) = True
isComment _           = False

isMacroLetter :: Char -> Bool
isMacroLetter c = isLetter c || c == '@'

isMacroSign :: Char -> Bool
isMacroSign c = isPrint c && not (isNumber c || isSpace c)

content :: TeXElement -> Text
content (Printable text)  = text
content (Macro text)      = text
content (Comment text)    = text
content _                 = empty

printable :: TeXElement -> Text
printable (Printable text)  = text
printable (Block tex)       = intercalate empty $ map printable tex
printable _                 = empty

body :: TeXElement -> TeX
body (Block tex)  = tex
body _            = []

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
