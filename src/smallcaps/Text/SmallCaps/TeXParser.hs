-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps.TeXParser
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This modules specifies parsers that consume 'Text' and produce a
-- 'TeXElement' token stream.
--
-------------------------------------------------------------------------------

module Text.SmallCaps.TeXParser where

import Data.Attoparsec.Text       ( Parser, satisfy, char, takeWhile1, takeTill, endOfLine, isEndOfLine )
import Data.Attoparsec.Combinator ( many', option )
import Data.Text                  ( Text, singleton, cons, snoc )
import Control.Monad              ( msum, mplus )

import Text.SmallCaps.TeX         ( TeX, TeXElement (..), isMacroLetter, isMacroSign )

tex :: Parser TeX
tex = many' $ msum
  [ printable
  , comment
  , macro
  , block
  , bblock
  ]

-- ** Printable

printable :: Parser TeXElement
printable = fmap Printable $ takeWhile1 printableChar
  where printableChar = not . flip elem "\\{}[]%"

-- ** Comment

comment :: Parser TeXElement
comment = fmap Comment $ do
  b <- commentChar
  c <- takeTill isEndOfLine
  option () endOfLine
  return (cons b (snoc c '\n'))

commentChar :: Parser Char
commentChar = char '%'

-- ** Macro

macro :: Parser TeXElement
macro = fmap Macro $ do
  b <- macroBegin
  n <- macroName
  return (cons b n)

macroBegin :: Parser Char
macroBegin = char '\\'

macroName :: Parser Text
macroName = macroLabel `mplus` tt macroSign

macroLabel :: Parser Text
macroLabel = takeWhile1 isMacroLetter

macroSign :: Parser Char
macroSign = satisfy isMacroSign

-- ** Block

block :: Parser TeXElement
block = fmap Block $ do
  _ <- blockBegin
  c <- tex
  _ <- blockEnd
  return c

blockBegin :: Parser Char
blockBegin = char '{'

blockEnd :: Parser Char
blockEnd = char '}'

-- ** BBlock

bblock :: Parser TeXElement
bblock = fmap BBlock $ do
  _ <- bblockBegin
  c <- tex
  _ <- bblockEnd
  return c

bblockBegin :: Parser Char
bblockBegin = char '['

bblockEnd :: Parser Char
bblockEnd = char ']'

-- ** Helpers

tt :: Parser Char -> Parser Text
tt = fmap singleton

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
