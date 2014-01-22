module Text.TeXParser where

import qualified  Data.Attoparsec.Text        as P
import            Data.Attoparsec.Text              ( Parser )
import            Data.Attoparsec.Combinator  as C
import qualified  Data.Text                   as T
import            Data.Text                         ( Text )
import            Control.Monad
import            Data.Char

import            Data.TeX

tex :: P.Parser TeX
tex = C.many' $ msum
  [ printable
  , comment
  , macro
  , block
  ]

printable :: Parser TeXElement
printable = fmap Printable $ P.takeWhile1 printableChar
  where printableChar = not . flip elem "\\{}%"

comment :: Parser TeXElement
comment = fmap Comment $ do
  b <- commentChar
  c <- P.takeTill P.isEndOfLine
  C.option () P.endOfLine
  return (T.cons b (T.snoc c '\n'))

macro :: Parser TeXElement
macro = fmap Macro $ do
  b <- macroBegin
  n <- macroName
  return (T.cons b n)

block :: Parser TeXElement
block = fmap Block $ do
  blockBegin
  c <- tex
  blockEnd
  return c

commentChar :: Parser Char
commentChar = P.char '%'

macroBegin :: Parser Char
macroBegin = P.char '\\'

macroName :: Parser Text
macroName = macroLabel `mplus` tt print'

macroLabel :: Parser Text
macroLabel = P.takeWhile1 isLabelLetter

blockBegin :: Parser Char
blockBegin = P.char '{'

blockEnd :: Parser Char
blockEnd = P.char '}'

print' :: Parser Char
print' = P.satisfy isPrint'
  where isPrint' c = isPrint c && not (isNumber c || isSpace c)

tt :: Parser Char -> Parser Text
tt = fmap T.singleton

isLabelLetter :: Char -> Bool
isLabelLetter c = isLetter c || c == '@'

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
