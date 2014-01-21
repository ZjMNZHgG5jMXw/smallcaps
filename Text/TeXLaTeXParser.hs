module Text.TeXLaTeXParser where

import Text.Parsec hiding ( satisfy )
import Data.Text ( Text, empty, pack, unpack, intercalate )
import Control.Monad

import qualified Data.TeX    as T
import qualified Data.LaTeX  as L

type Parser       = Parsec T.TeX ()
type LaTeX        = L.LaTeX
type LaTeXElement = L.LaTeXElement

latex :: Parser LaTeX
latex = many $ msum
  [ environment
  , macro
  , latexElement
  ]

latexElement :: Parser LaTeXElement
latexElement = satisfy (const True)

satisfy :: (T.TeXElement -> Bool) -> Parser LaTeXElement
satisfy pass = tokenPrim show updpos get where
  get x@(T.Printable text)  | pass x  = Just $ L.Printable text
  get x@(T.Macro text)      | pass x  = Just $ L.Macro text [] -- use macro instead!
  get x@(T.Block tex)       | pass x  = Just $ L.Block $ either (const []) id $ parse latex "" tex
  get x@(T.Comment text)    | pass x  = Just $ L.Comment text
  get _                               = Nothing

skipMacro :: Text -> Parser ()
skipMacro name = tokenPrim show updpos get where
  get (T.Macro name') | name == name' = Just ()
  get _                               = Nothing

macroSatisfy :: (T.TeXElement -> Bool) -> Parser LaTeXElement
macroSatisfy cond = satisfy (\x -> isMacro x && cond x) >>= \(L.Macro name _) -> fmap (L.Macro name) (many anyBlock)

macro :: Parser LaTeXElement
macro = macroSatisfy (const True)

anyBlock :: Parser LaTeXElement
anyBlock = satisfy isBlock

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

macroTextArg :: Text -> Parser Text -- ^ compares the first printable to the text only
macroTextArg name = do
  skipMacro name
  (L.Block latex) <- anyBlock
  case latex of
    (L.Printable text:_)  -> return text
    _                     -> return empty

beginEnv :: Parser Text
beginEnv = macroTextArg (pack "\\begin")

endEnv :: Parser Text
endEnv = macroTextArg (pack "\\end")

environment :: Parser LaTeXElement
environment = do
  nameB <- beginEnv
  latex <- many (environment `mplus` macroSatisfy (not . isEndEnv) `mplus` satisfy (not . isEndEnv))
  nameE <- endEnv
  if nameB == nameE
  then return (L.Environment nameB latex)
  else fail ("\\end{" ++ unpack nameB ++ "} expected. found " ++ unpack nameE)

isMacro :: T.TeXElement -> Bool
isMacro (T.Macro _) = True
isMacro _           = False

isBlock :: T.TeXElement -> Bool
isBlock (T.Block _) = True
isBlock _           = False

isEndEnv :: T.TeXElement -> Bool
isEndEnv (T.Macro name) = name == pack "\\end"
isEndEnv _              = False

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
