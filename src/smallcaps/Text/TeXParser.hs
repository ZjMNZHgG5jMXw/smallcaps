module Text.TeXParser where

import Data.Attoparsec.Text       ( Parser, satisfy, char, takeWhile1, takeTill, endOfLine, isEndOfLine )
import Data.Attoparsec.Combinator ( many', option )
import Data.Text                  ( Text, singleton, cons, snoc )
import Control.Monad              ( msum, mplus )

import Data.TeX                   ( TeX, TeXElement (..), isMacroLetter, isMacroSign )

tex :: Parser TeX
tex = many' $ msum
  [ printable
  , comment
  , macro
  , block
  ]

printable :: Parser TeXElement
printable = fmap Printable $ takeWhile1 printableChar
  where printableChar = not . flip elem "\\{}%"

comment :: Parser TeXElement
comment = fmap Comment $ do
  b <- commentChar
  c <- takeTill isEndOfLine
  option () endOfLine
  return (cons b (snoc c '\n'))

macro :: Parser TeXElement
macro = fmap Macro $ do
  b <- macroBegin
  n <- macroName
  return (cons b n)

block :: Parser TeXElement
block = fmap Block $ do
  _ <- blockBegin
  c <- tex
  _ <- blockEnd
  return c

commentChar :: Parser Char
commentChar = char '%'

macroBegin :: Parser Char
macroBegin = char '\\'

macroName :: Parser Text
macroName = macroLabel `mplus` tt macroSign

macroLabel :: Parser Text
macroLabel = takeWhile1 isMacroLetter

blockBegin :: Parser Char
blockBegin = char '{'

blockEnd :: Parser Char
blockEnd = char '}'

macroSign :: Parser Char
macroSign = satisfy isMacroSign

tt :: Parser Char -> Parser Text
tt = fmap singleton

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
