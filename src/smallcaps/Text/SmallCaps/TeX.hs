-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps.TeX
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This modules specifies the data types 'TeX' and 'TeXElement'.
--
-------------------------------------------------------------------------------

module Text.SmallCaps.TeX where

import Data.Text ( Text, empty, intercalate )
import Data.Char ( isPrint, isNumber, isSpace, isLetter )

type TeX = [TeXElement]

data TeXElement
  = Printable Text
  | Macro Text
  | Block TeX
  | BBlock TeX
  | Comment Text
  deriving (Eq, Show)

-- ** Query

isPrintable :: TeXElement -> Bool
isPrintable (Printable _) = True
isPrintable _             = False

isMacro :: TeXElement -> Bool
isMacro (Macro _) = True
isMacro _         = False

isBlock :: TeXElement -> Bool
isBlock (Block _) = True
isBlock _         = False

isBBlock :: TeXElement -> Bool
isBBlock (BBlock _) = True
isBBlock _          = False

isComment :: TeXElement -> Bool
isComment (Comment _) = True
isComment _           = False

isMacroLetter :: Char -> Bool
isMacroLetter c = isLetter c || c == '@'

isMacroSign :: Char -> Bool
isMacroSign c = isPrint c && not (isNumber c || isSpace c)

-- ** Accessors

content :: TeXElement -> Text
content (Printable text)  = text
content (Macro text)      = text
content (Comment text)    = text
content _                 = empty

printable :: TeXElement -> Text
printable (Printable text)  = text
printable (Block tex)       = intercalate empty $ map printable tex
printable (BBlock tex)      = intercalate empty $ map printable tex
printable _                 = empty

body :: TeXElement -> TeX
body (Block tex)  = tex
body (BBlock tex) = tex
body _            = []

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
