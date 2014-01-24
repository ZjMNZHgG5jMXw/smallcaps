module Text.PrintableParser where

import            Prelude      hiding ( head, tail, null )

import            Text.Parsec         ( runParser, oneOf, anyChar, many, many1, lower, upper, getState, modifyState )
import qualified  Text.Parsec    as P ( space, newline )
import            Text.Parsec.Text    ( GenParser )
import            Data.Text           ( Text, null, empty, singleton, pack, head, tail, append, intercalate )
import            Control.Monad       ( msum )

import            Data.Config         ( Config (..), StopState (..), ParserState (..), SubParser )

type Parser = GenParser ParserState

runPrintableWith :: SubParser Text
runPrintableWith state = either (Left . show) Right . runParser (printable >>= \a -> fmap ((,) a) getState) state ""

printable :: Parser Text
printable = fmap (intercalate (pack "")) $ many $ printableElement

printableElement :: Parser Text
printableElement = msum
  [ lowers
  , uppers
  , period
  , newline
  , space
  , misc
  ] 

lowers :: Parser Text
lowers = fmap pack $ many1 lower >>= pass reset

uppers :: Parser Text
uppers = do
  text <- fmap pack $ many1 upper
  state <- getState
  if ignore state
  then return text >>= pass reset
  else do
    let (h,t) = uc text (stop state)
    pass reset $ h `append` replace' (config state) t

period :: Parser Text
period = do
  ps <- fmap (periodChars . config) getState
  fmap singleton $ oneOf ps >>= pass set

space :: Parser Text
space = fmap singleton $ P.space >>= pass sticky

newline :: Parser Text
newline = fmap singleton $ P.newline >>= pass inc

misc :: Parser Text
misc = fmap singleton $ anyChar >>= pass reset

pass :: Parser b -> a -> Parser a
pass m a = m >> return a

reset :: Parser ()
reset = modifyState (\state -> state { stop = None })

set :: Parser ()
set = modifyState (\state -> state { stop = Stop })

inc :: Parser ()
inc = modifyState (\state -> state { stop = inc' (stop state) }) where
  inc' None = NewLine
  inc' _    = NewSentence

sticky :: Parser ()
sticky = modifyState (\state -> state { stop = inc' (stop state) }) where
  inc' None     = None
  inc' NewLine  = NewLine
  inc' _        = NewSentence

uc :: Text -> StopState -> (Text, Text)
uc text state
  | state == NewSentence  = (singleton (head text), tail text)
  | otherwise             = (empty, text)

replace' :: Config -> Text -> Text
replace' conf text
  | null text = text
  | otherwise = replace conf text

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
