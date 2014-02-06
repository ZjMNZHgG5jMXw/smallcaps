module SmallCaps.LaTeXParser where

import Text.Parsec      ( Parsec, SourcePos, tokenPrim )
import Data.Text        ( Text )
import Control.Monad    ( liftM2 )

import SmallCaps.LaTeX  ( LaTeX, LaTeXElement
                        , isMacro, isEnvironment, isBlock, isPrintable, isComment
                        , name, content
                        )

type Parser u = Parsec LaTeX u

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

anyComment :: Parser u LaTeXElement
anyComment = satisfy isComment

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
