module Text.ConfigParser where

import Prelude hiding ( lex, takeWhile )

import Data.Char                  ( isSpace, isAlpha, isNumber, isPunctuation, isPrint )
import Data.Text hiding           ( replace, takeWhile )
import Data.Attoparsec.Text       ( Parser, parseOnly, satisfy, char, takeWhile1, asciiCI, skipSpace )
import Data.Attoparsec.Combinator ( many' )
import Data.Default               ( def )
import Control.Monad              ( mplus, msum )

import Data.LaTeX                 ( LaTeXElement ( Macro, Environment ) )
import Data.Config                ( Config (..), StopState (..), clean, conservative, busy, blacklist, whitelist )

reconfigure :: Config -> Text -> Maybe Config
reconfigure conf = either (const Nothing) Just . parseOnly (reconfiguration conf)

reconfiguration :: Config -> Parser Config
reconfiguration conf = preamble >> msum
  [ profileMain
  , periodMain  conf
  , replaceMain conf
  , searchMain  conf
  , isolateMain conf
  , skipMain    conf
  , unskipMain  conf
  , eosMain     conf
  ]

-- Lexer

lex :: Parser a -> Parser a
lex p = skipSpace >> p

-- Preamble

preamble :: Parser Text
preamble = char '%' >> lex (asciiCI (pack "smallcaps"))

-- Policy

profileMain :: Parser Config
profileMain = profilePre >> msum
  [ profileDefault
  , profileClean
  , profileConservative
  , profileBusy
  ]

profilePre :: Parser Text
profilePre = lex (asciiCI (pack "reset")) >> lex (asciiCI (pack "profile"))

profileDefault :: Parser Config
profileDefault = lex $ asciiCI (pack "default") >> return def

profileClean :: Parser Config
profileClean = lex $ asciiCI (pack "clean") >> return clean

profileConservative :: Parser Config
profileConservative = lex $ asciiCI (pack "conservative") >> return conservative

profileBusy :: Parser Config
profileBusy = lex $ asciiCI (pack "busy") >> return busy

-- Period chars

periodMain :: Config -> Parser Config
periodMain = (periodPre >>) . periodSigns

periodPre :: Parser Text
periodPre = lex (asciiCI (pack "periods")) >> lex (asciiCI (pack "are"))

periodSigns :: Config -> Parser Config
periodSigns conf = lex (takeWhile1 isPunctuation) >>= \s -> return $ conf { periodChars = unpack s }

-- Replace string

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
  | style == NoArg = fun (\macro caps -> pack "{\\" `append` macro `append` cons ' ' (snoc caps '}'))
  | style == InArg = fun (\macro caps -> cons '\\' macro `append` cons '{' (snoc caps '}'))
  where fun gun = lex $ char '\\' >> takeWhile1 isAlpha >>= \macro -> return $ conf { replace = gun macro }

-- Search filter

searchMain :: Config -> Parser Config
searchMain = (searchPre >>) . searchList

searchPre :: Parser Text
searchPre = lex $ asciiCI (pack "search")

searchList :: Config -> Parser Config
searchList conf = list' (search conf) >>= \fun -> return $ conf { search = fun }

-- Isolate filter

isolateMain :: Config -> Parser Config
isolateMain = (isolatePre >>) . isolateList

isolatePre :: Parser Text
isolatePre = lex $ asciiCI (pack "isolate")

isolateList :: Config -> Parser Config
isolateList conf = list (isolate conf) >>= \fun -> return $ conf { isolate = fun }

-- Skip filter

skipMain :: Config -> Parser Config
skipMain = (skipPre >>) . skipList

skipPre :: Parser Text
skipPre = lex $ asciiCI (pack "skip")

skipList :: Config -> Parser Config
skipList conf = list (skip conf) >>= \fun -> return $ conf { skip = fun }

-- Unskip filter

unskipMain :: Config -> Parser Config
unskipMain = (unskipPre >>) . unskipList

unskipPre :: Parser Text
unskipPre = lex $ asciiCI (pack "unskip")

unskipList :: Config -> Parser Config
unskipList conf = list (unskip conf) >>= \fun -> return $ conf { unskip = fun }

-- End of sentence filter

eosMain :: Config -> Parser Config
eosMain = (eosPre >>) . eosList

eosPre :: Parser Text
eosPre = lex $ asciiCI (pack "eos")

eosList :: Config -> Parser Config
eosList conf = list (eos conf) >>= \fun -> return $ conf { eos = fun }

-- Macro/environment name list parser

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

listItems :: Parser [Text]
listItems = do
  x <- listItem
  xs <- many' (listItemSeparator >> listItem)
  return (x:xs)

listItem :: Parser Text
listItem = listItemMacro `mplus` listItemEnvironment

listItemMacro :: Parser Text
listItemMacro = lex (char '\\' >> fmap (cons '\\') macroName)
  where macroName   = takeWhile1 isAlpha `mplus` fmap singleton (satisfy isPrint')
        isPrint' c  = isPrint c && not (isNumber c || isSpace c)

listItemEnvironment :: Parser Text
listItemEnvironment = lex (takeWhile1 isAlpha)

listItemSeparator :: Parser Char
listItemSeparator = lex $ char ','

name :: LaTeXElement -> Text
name (Macro n _)        = n
name (Environment n _)  = n
name _                  = empty

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
