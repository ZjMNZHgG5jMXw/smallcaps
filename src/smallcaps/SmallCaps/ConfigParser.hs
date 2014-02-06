-------------------------------------------------------------------------------
-- |
-- Module      :  SmallCaps.ConfigParser
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module specifies inline configuration parsers. The parsers are also
-- used for the arguments in the command line interface.
--
-------------------------------------------------------------------------------

module SmallCaps.ConfigParser where

import Prelude hiding ( lex, takeWhile )

import Data.Char                  ( isAlpha, isAlphaNum, isPunctuation )
import Data.Text hiding           ( replace, takeWhile )
import Data.Map                   ( Map )
import qualified Data.Map as Map  ( lookup )
import Data.Attoparsec.Text       ( Parser, parseOnly, char, takeWhile1, asciiCI, skipSpace )
import Data.Attoparsec.Combinator ( many' )
import Control.Monad              ( mplus, msum )

import SmallCaps.LaTeX            ( LaTeXElement, name )
import SmallCaps.Config           ( ParserState (..), Config (..), blacklist, whitelist )
import SmallCaps.TeXParser        ( macroBegin, macroName )

reconfigure :: ParserState -> Text -> Either (Text, Config) Config
reconfigure state = either (const (Right (config state))) id . parseOnly (reconfiguration state)

reconfiguration :: ParserState -> Parser (Either (Text, Config) Config)
reconfiguration state = preamble >> msum
  [ fmap Right  $ profileMain (profile state)
  , fmap Left   $ storeMain   conf
  , fmap Right  $ periodMain  conf
  , fmap Right  $ replaceMain conf
  , fmap Right  $ searchMain  conf
  , fmap Right  $ isolateMain conf
  , fmap Right  $ skipMain    conf
  , fmap Right  $ unskipMain  conf
  , fmap Right  $ eosMain     conf
  ] where conf = config state

-- ** Lexer

lex :: Parser a -> Parser a
lex p = skipSpace >> p

-- ** Preamble

preamble :: Parser Text
preamble = char '%' >> lex (asciiCI (pack "smallcaps"))

-- ** Restore profile

profileMain :: Map Text Config -> Parser Config
profileMain ps = profilePre >> profileName ps

profilePre :: Parser Text
profilePre = lex (asciiCI (pack "reset") `mplus` asciiCI (pack "restore")) >> lex (asciiCI (pack "profile"))

profileName :: Map Text Config -> Parser Config
profileName ps = maybe (fail "profile not found") return . flip Map.lookup ps =<< lex (takeWhile1 isAlphaNum)

-- ** Store profile

storeMain :: Config -> Parser (Text, Config)
storeMain = (storePre >>) . storeName

storePre :: Parser Text
storePre = lex (asciiCI (pack "store")) >> lex (asciiCI (pack "profile"))

storeName :: Config -> Parser (Text, Config)
storeName conf = fmap (flip (,) conf) (lex $ takeWhile1 isAlphaNum)

-- ** Period chars

periodMain :: Config -> Parser Config
periodMain = (periodPre >>) . periodSigns

periodPre :: Parser Text
periodPre = lex (asciiCI (pack "periods")) >> lex (asciiCI (pack "are"))

periodSigns :: Config -> Parser Config
periodSigns conf = lex (takeWhile1 isPunctuation) >>= \s -> return $ conf { periodChars = unpack s }

-- ** Replace string

replaceMain :: Config -> Parser Config
replaceMain conf = replacePre >> msum
  [ replaceStyleNoarg
  , replaceStyleInarg
  ] >>= replaceMacro conf

replacePre :: Parser Text
replacePre = lex $ asciiCI (pack "substitution")

data Style = NoArg | InArg deriving (Show, Eq)

replaceStyleNoarg :: Parser Style
replaceStyleNoarg = lex (asciiCI (pack "in")) >> lex (asciiCI (pack "block")) >> lex (asciiCI (pack "with")) >> return NoArg

replaceStyleInarg :: Parser Style
replaceStyleInarg = lex (asciiCI (pack "as")) >> lex (asciiCI (pack "argument")) >> lex (asciiCI (pack "of")) >> return InArg

replaceMacro :: Config -> Style -> Parser Config
replaceMacro conf style
  | style == NoArg  = fun (\macro caps -> pack "{\\" `append` macro `append` cons ' ' (snoc caps '}'))
  | otherwise       = fun (\macro caps -> cons '\\' macro `append` cons '{' (snoc caps '}'))
  where fun gun = lex $ macroBegin >> macroName >>= \macro -> return $ conf { replace = gun macro }

-- ** Search filter

searchMain :: Config -> Parser Config
searchMain = (searchPre >>) . searchList

searchPre :: Parser Text
searchPre = lex $ asciiCI (pack "search")

searchList :: Config -> Parser Config
searchList conf = list' (search conf) >>= \fun -> return $ conf { search = fun }

-- ** Isolate filter

isolateMain :: Config -> Parser Config
isolateMain = (isolatePre >>) . isolateList

isolatePre :: Parser Text
isolatePre = lex $ asciiCI (pack "isolate")

isolateList :: Config -> Parser Config
isolateList conf = iList (isolate conf) >>= \fun -> return $ conf { isolate = fun }

-- ** Skip filter

skipMain :: Config -> Parser Config
skipMain = (skipPre >>) . skipList

skipPre :: Parser Text
skipPre = lex $ asciiCI (pack "skip")

skipList :: Config -> Parser Config
skipList conf = list (skip conf) >>= \fun -> return $ conf { skip = fun }

-- ** Unskip filter

unskipMain :: Config -> Parser Config
unskipMain = (unskipPre >>) . unskipList

unskipPre :: Parser Text
unskipPre = lex $ asciiCI (pack "unskip")

unskipList :: Config -> Parser Config
unskipList conf = list (unskip conf) >>= \fun -> return $ conf { unskip = fun }

-- ** End of sentence filter

eosMain :: Config -> Parser Config
eosMain = (eosPre >>) . eosList

eosPre :: Parser Text
eosPre = lex $ asciiCI (pack "eos")

eosList :: Config -> Parser Config
eosList conf = list (eos conf) >>= \fun -> return $ conf { eos = fun }

-- ** Macro/environment name list parser

list :: (LaTeXElement -> Bool) -> Parser (LaTeXElement -> Bool)
list fun = msum [listBlack fun, listWhite fun, listConstAll, listConstNone]

list' :: (LaTeXElement -> Bool) -> Parser (LaTeXElement -> Bool)
list' fun = msum [listBlack fun, listWhite fun, listConstAll', listConstNone']

listBlack :: (LaTeXElement -> Bool) -> Parser (LaTeXElement -> Bool)
listBlack fun = lex (char '-') >> listItems >>= \xs -> return (\x -> not (name x `elem` xs) && fun x)

listWhite :: (LaTeXElement -> Bool) -> Parser (LaTeXElement -> Bool)
listWhite fun = lex $ char '+' >> listItems >>= \xs -> return (\x -> name x `elem` xs || fun x)

listConstAll :: Parser (a -> Bool)
listConstAll = lex (char '*') >> return (const True)

listConstAll' :: Parser (LaTeXElement -> Bool)
listConstAll' = lex (char '*') >> return (blacklist [])

listConstNone :: Parser (a -> Bool)
listConstNone = lex (char '/') >> return (const False)

listConstNone' :: Parser (LaTeXElement -> Bool)
listConstNone' = lex (char '/') >> return (whitelist [])

-- ** Isolate list parser

iList :: (LaTeXElement -> Maybe Text) -> Parser (LaTeXElement -> Maybe Text)
iList fun = msum [iListBlack fun, iListWhite fun, iListConstAll, iListConstNone]

iListBlack :: (LaTeXElement -> Maybe Text) -> Parser (LaTeXElement -> Maybe Text)
iListBlack fun = do
  _   <- lex $ char '-'
  xs  <- listItems
  return $ \x ->  if x `isElement` xs
                  then Nothing
                  else fun x

iListWhite :: (LaTeXElement -> Maybe Text) -> Parser (LaTeXElement -> Maybe Text)
iListWhite fun = do
  c   <- lex $ takeWhile1 isAlphaNum `mplus` return (pack "default")
  _   <- lex $ char '+'
  xs  <- listItems
  return $ \x ->  if x `isElement` xs
                  then Just c
                  else fun x

iListConstAll :: Parser (LaTeXElement -> Maybe Text)
iListConstAll = do
  c   <- lex $ takeWhile1 isAlphaNum `mplus` return (pack "default")
  _   <- lex $ char '*' 
  return $ const (Just c)

iListConstNone :: Parser (LaTeXElement -> Maybe Text)
iListConstNone = do
  _   <- lex $ char '/'
  return $ const Nothing

-- ** List item parser

listItems :: Parser [Text]
listItems = do
  x   <- listItem
  xs  <- many' (listItemSeparator >> listItem)
  return (x:xs)

listItem :: Parser Text
listItem = listItemMacro `mplus` listItemEnvironment

listItemMacro :: Parser Text
listItemMacro = lex (macroBegin >> fmap (cons '\\') macroName)

listItemEnvironment :: Parser Text
listItemEnvironment = lex (takeWhile1 isAlpha)

listItemSeparator :: Parser Char
listItemSeparator = lex $ char ','

isElement :: LaTeXElement -> [Text] -> Bool
isElement = elem . name

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
