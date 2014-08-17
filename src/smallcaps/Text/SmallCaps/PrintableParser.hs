-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps.PrintableParser
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This modules specifies parsers on printable 'Text'.
--
-------------------------------------------------------------------------------

module Text.SmallCaps.PrintableParser where

import            Prelude      hiding ( head, tail, null )

import            Text.Parsec         ( runParser, try, oneOf, anyChar, many, many1, lower, upper, string, getState, modifyState )
import qualified  Text.Parsec    as P ( space, newline )
import            Text.Parsec.Text    ( GenParser )
import            Data.Text           ( Text, null, empty, singleton, pack, unpack, head, tail, append, intercalate )
import            Control.Monad       ( msum )

import            Text.SmallCaps.Config ( Config (..), StopState (..), ParserState (..), SubParser, PatternReplace (..) )

type Parser = GenParser ParserState

runPrintableWith :: SubParser Text
runPrintableWith state = either (Left . show) Right . runParser (printable >>= \a -> fmap ((,) a) getState) state ""

-- ** Parsers

printable :: Parser Text
printable = fmap (intercalate (pack "")) $ many $ printableElement

printableElement :: Parser Text
printableElement = msum
  [ excepts
  , lowers
  , uppers
  , period
  , newline
  , space
  , misc
  ] 

excepts :: Parser Text
excepts = msum =<< fmap (map toParser . exceptions . config) getState
  where toParser x = try (string (unpack $ pattern x)) >> return (replacement x) >>= pass reset

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

-- ** State modification

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

-- ** Text modification

replace' :: Config -> Text -> Text
replace' conf text
  | null text = text
  | otherwise = replace conf text

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
