module Text.LaTeXParser where

import Text.Parsec  ( Parsec, SourcePos, tokenPrim )
import Data.Text    ( Text, empty, pack, unpack, intercalate )

import Data.LaTeX   ( LaTeX, LaTeXElement (..) )

type Parser u = Parsec LaTeX u

satisfy :: (LaTeXElement -> Bool) -> Parser u LaTeXElement
satisfy pass = tokenPrim show updpos get where
  get x | pass x  = Just x
  get _           = Nothing

anyPrintable :: Parser u LaTeXElement
anyPrintable = satisfy isPrintable

printable :: Text -> Parser u LaTeXElement
printable text = tokenPrim show updpos get where
  get x@(Printable text') | text == text' = Just x
  get _                                   = Nothing

anyMacro :: Parser u LaTeXElement
anyMacro = satisfy isMacro

macro :: Text -> Parser u LaTeXElement
macro name = tokenPrim show updpos get where
  get x@(Macro name' _) | name == name' = Just x
  get _                                 = Nothing

anyEnvironment :: Parser u LaTeXElement
anyEnvironment = satisfy isEnvironment

environment :: Text -> Parser u LaTeXElement
environment name = tokenPrim show updpos get where
  get x@(Environment name' _) | name == name' = Just x
  get _                                       = Nothing

anyBlock :: Parser u LaTeXElement
anyBlock = satisfy isBlock

anyComment :: Parser u LaTeXElement
anyComment = satisfy isComment

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

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

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
