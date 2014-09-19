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
import            Data.Text           ( Text, singleton, pack, unpack, intercalate )
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
  where toParser x = try (string (unpack $ pattern x)) >> pass reset (replacement x)

lowers :: Parser Text
lowers =  pass reset . pack =<< many1 lower

uppers :: Parser Text
uppers = do
  text <- fmap pack $ many1 upper
  state <- getState
  if ignore state || not (replaceFilter (config state) text)
  then pass reset text
  else pass reset $ replace (config state) (stop state) text

period :: Parser Text
period = do
  ps <- fmap (periodChars . config) getState
  pass set . singleton =<< oneOf ps

space :: Parser Text
space =  pass sticky . singleton =<< P.space

newline :: Parser Text
newline = pass inc . singleton =<< P.newline

misc :: Parser Text
misc = pass reset . singleton =<< anyChar

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

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
