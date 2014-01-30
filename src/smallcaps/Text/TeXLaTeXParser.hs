module Text.TeXLaTeXParser where

import Text.Parsec    ( Parsec, SourcePos, parse, tokenPrim, many )
import Data.Text      ( Text, empty, pack, unpack, intercalate )
import Control.Monad  ( liftM2, mplus, msum )

import Data.TeX       ( TeX, TeXElement
                      , isPrintable, isMacro, isBlock, isComment
                      , content, body
                      )
import Data.LaTeX     ( LaTeX, LaTeXElement (..) )

type Parser       = Parsec TeX ()

latex :: Parser LaTeX
latex = many $ msum
  [ environment
  , macro
  , latexElement
  ]

-- TeXElement

satisfy :: (TeXElement -> Bool) -> Parser TeXElement
satisfy pass = tokenPrim show updpos get where
  get x | pass x    = Just x
        | otherwise = Nothing

skipMacro :: Text -> Parser TeXElement
skipMacro name = satisfy (liftM2 (&&) isMacro ((name ==) . content))

-- LaTeXElement

translate :: TeXElement -> LaTeXElement
translate x
  | isPrintable x = Printable (content x)
  | isMacro     x = Macro     (content x) [] -- use macro instead!
  | isComment   x = Comment   (content x)
  | otherwise     = Block $ either (const []) id $ parse latex "" (body x)

macroSatisfy :: (TeXElement -> Bool) -> Parser LaTeXElement
macroSatisfy cond = satisfy (liftM2 (&&) isMacro cond) >>= \x -> fmap (Macro (content x)) (many anyBlock)

macro :: Parser LaTeXElement
macro = macroSatisfy (const True)

macroTextArg :: Text -> Parser Text
macroTextArg name = skipMacro name >> fmap arg (satisfy isBlock)
  where arg = intercalate empty . map content . filter isPrintable . body

environment :: Parser LaTeXElement
environment = do
  nameB   <- beginEnv
  latex'  <- many (environment `mplus` macroSatisfy (not . isEndEnv) `mplus` fmap translate (satisfy (not . isEndEnv)))
  nameE   <- endEnv
  if nameB == nameE
  then return (Environment nameB latex')
  else fail ("\\end{" ++ unpack nameB ++ "} expected. found " ++ unpack nameE)

anyBlock :: Parser LaTeXElement
anyBlock = fmap translate (satisfy isBlock)

latexElement :: Parser LaTeXElement
latexElement = fmap translate (satisfy (const True))

beginEnv :: Parser Text
beginEnv = macroTextArg (pack "\\begin")

endEnv :: Parser Text
endEnv = macroTextArg (pack "\\end")

isEndEnv :: TeXElement -> Bool
isEndEnv x = isMacro x && content x == pack "\\end"

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
