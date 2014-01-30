module Text.TeXLaTeXParser where

import Text.Parsec    ( Parsec, SourcePos, parse, tokenPrim, many )
import Data.Text      ( Text, empty, pack, unpack )
import Control.Monad  ( liftM2, mplus, msum )

import qualified Data.TeX    as T ( TeX, TeXElement
                                  , isPrintable, isMacro, isBlock, isComment
                                  , content, body
                                  )
import qualified Data.LaTeX  as L ( LaTeXElement (..) )
import           Data.LaTeX       ( LaTeX, LaTeXElement )

type Parser       = Parsec T.TeX ()

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
  get x | T.isPrintable x && pass x = Just $ L.Printable  (T.content x)
        | T.isMacro x     && pass x = Just $ L.Macro      (T.content x) [] -- use macro instead!
        | T.isBlock x     && pass x = Just $ L.Block $ either (const []) id $ parse latex "" (T.body x)
        | T.isComment x   && pass x = Just $ L.Comment    (T.content x)
        | otherwise                 = Nothing

skipMacro :: Text -> Parser LaTeXElement
skipMacro name = satisfy (liftM2 (&&) T.isMacro ((name ==) . T.content))

macroSatisfy :: (T.TeXElement -> Bool) -> Parser LaTeXElement
macroSatisfy cond = satisfy (liftM2 (&&) T.isMacro cond) >>= \(L.Macro name _) -> fmap (L.Macro name) (many anyBlock)

macro :: Parser LaTeXElement
macro = macroSatisfy (const True)

anyBlock :: Parser LaTeXElement
anyBlock = satisfy T.isBlock

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

macroTextArg :: Text -> Parser Text -- ^ matches macro name and returns the first printable to the text only
macroTextArg name = do
  _ <- skipMacro name
  (L.Block latex') <- anyBlock
  case latex' of
    (L.Printable text:_)  -> return text
    _                     -> return empty

beginEnv :: Parser Text
beginEnv = macroTextArg (pack "\\begin")

endEnv :: Parser Text
endEnv = macroTextArg (pack "\\end")

environment :: Parser LaTeXElement
environment = do
  nameB <- beginEnv
  latex' <- many (environment `mplus` macroSatisfy (not . isEndEnv) `mplus` satisfy (not . isEndEnv))
  nameE <- endEnv
  if nameB == nameE
  then return (L.Environment nameB latex')
  else fail ("\\end{" ++ unpack nameB ++ "} expected. found " ++ unpack nameE)

isEndEnv :: T.TeXElement -> Bool
isEndEnv x = T.isMacro x && T.content x == pack "\\end"

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
