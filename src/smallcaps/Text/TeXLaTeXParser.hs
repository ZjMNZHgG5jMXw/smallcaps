module Text.TeXLaTeXParser where

import Text.Parsec                ( ParsecT, runParserT, SourcePos, ParseError, tokenPrim, many )
import Data.Text                  ( Text, empty, pack, unpack, intercalate )
import Control.Monad              ( liftM2, mplus, msum )
import Control.Monad.Trans.Writer ( WriterT, Writer, runWriter, tell )
import Control.Monad.Trans.Class  ( lift )
import Control.Arrow              ( first )

import Data.TeX                   ( TeX, TeXElement
                                  , isPrintable, isMacro, isBlock, isComment
                                  , content
                                  )
import qualified Data.TeX   as T  ( body )
import Data.LaTeX                 ( LaTeX, LaTeXElement (..), name, printable )
import qualified Data.LaTeX as L  ( body )

type Parser       = ParsecT TeX () (Writer [Text])

parse :: Parser [a] -> TeX -> ([a], [Text])
parse = (first (either (const []) id) .) . parse'

parse' :: Parser a -> TeX -> (Either ParseError a, [Text])
parse' = (runWriter .) . flip (flip runParserT ()) ""

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
skipMacro name' = satisfy (liftM2 (&&) isMacro ((name' ==) . content))

-- LaTeXElement

translate :: TeXElement -> (LaTeXElement, [Text])
translate x
  | isPrintable x = (Printable  (content x),    [])
  | isMacro     x = (Macro      (content x) [], []) -- use macro instead!
  | isComment   x = (Comment    (content x),    [])
  | otherwise     = first Block $ parse latex (T.body x)

translateTell :: Monad m => TeXElement -> WriterT [Text] m LaTeXElement
translateTell = uncurry (flip ((>>) . tell) . return) . translate

macroSatisfy :: (TeXElement -> Bool) -> Parser LaTeXElement
macroSatisfy cond = satisfy (liftM2 (&&) isMacro cond) >>= \x -> fmap (Macro (content x)) (many anyBlock)

macro :: Parser LaTeXElement
macro = do
  x <- macroSatisfy (const True)
  if (name x == pack "\\include") || (name x == pack "\\input")
  then lift $ tell [intercalate empty $ map printable $ L.body x]
  else return ()
  return x

macroTextArg :: Text -> Parser Text
macroTextArg name' = skipMacro name' >> fmap arg (satisfy isBlock)
  where arg = intercalate empty . map content . filter isPrintable . T.body

environment :: Parser LaTeXElement
environment = do
  nameB   <- beginEnv
  latex'  <- many (environment `mplus` macroSatisfy (not . isEndEnv) `mplus` (lift . translateTell =<< satisfy (not . isEndEnv)))
  nameE   <- endEnv
  if nameB == nameE
  then return (Environment nameB latex')
  else fail ("\\end{" ++ unpack nameB ++ "} expected. found " ++ unpack nameE)

anyBlock :: Parser LaTeXElement
anyBlock = lift . translateTell =<< satisfy isBlock

latexElement :: Parser LaTeXElement
latexElement = lift . translateTell =<< satisfy (const True)

beginEnv :: Parser Text
beginEnv = macroTextArg (pack "\\begin")

endEnv :: Parser Text
endEnv = macroTextArg (pack "\\end")

isEndEnv :: TeXElement -> Bool
isEndEnv x = isMacro x && content x == pack "\\end"

updpos :: SourcePos -> t -> s -> SourcePos
updpos pos _ _ = pos

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
