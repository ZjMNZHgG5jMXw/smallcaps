-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps.LaTeXParser
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This modules specifies parsers on 'LaTeXElement' token streams.
--
-------------------------------------------------------------------------------

module Text.SmallCaps.LaTeXParser where

import Text.Parsec      ( Parsec, SourcePos, tokenPrim )
import Data.Text        ( Text )
import Control.Monad    ( liftM2 )

import Text.SmallCaps.LaTeX ( LaTeX, LaTeXElement
                            , isMacro, isEnvironment, isBlock, isBBlock, isMath, isPrintable, isComment
                            , name, content
                            )

type Parser u = Parsec LaTeX u

-- ** Parsers

satisfy :: (LaTeXElement -> Bool) -> Parser u LaTeXElement
satisfy pass = tokenPrim show updpos get where
  get x | pass x  = Just x
  get _           = Nothing

anyPrintable :: Parser u LaTeXElement
anyPrintable = satisfy isPrintable

printable :: Text -> Parser u LaTeXElement
printable text = satisfy (liftM2 (&&) isPrintable ((text ==) . content))

anyMacro :: Parser u LaTeXElement
anyMacro = satisfy isMacro

macro :: Text -> Parser u LaTeXElement
macro n = satisfy (liftM2 (&&) isMacro ((n ==) . name))

anyEnvironment :: Parser u LaTeXElement
anyEnvironment = satisfy isEnvironment

environment :: Text -> Parser u LaTeXElement
environment n = satisfy (liftM2 (&&) isEnvironment ((n ==) . name))

anyBlock :: Parser u LaTeXElement
anyBlock = satisfy isBlock

anyBBlock :: Parser u LaTeXElement
anyBBlock = satisfy isBBlock

anyMath :: Parser u LaTeXElement
anyMath = satisfy isMath

anyComment :: Parser u LaTeXElement
anyComment = satisfy isComment

-- ** Helpers

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
