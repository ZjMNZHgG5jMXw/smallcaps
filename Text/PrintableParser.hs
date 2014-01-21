module Text.PrintableParser where

import Prelude hiding ( head, tail, null )

import Text.Parsec hiding ( space, newline )
import qualified Text.Parsec as P
import Text.Parsec.Text ( GenParser )
import Data.Text hiding ( replace )
import Data.Char
import Data.Default
import Control.Monad

import Data.Config
import Data.StopState

type Parser = GenParser StopState

runPrintableWith :: Config -> SubParser Text
runPrintableWith conf state = either (error . show) id . runParser (printable conf >>= \a -> fmap ((,) a) getState) state ""

printable :: Config -> Parser Text
printable = fmap (intercalate (pack "")) . many . printableElement

printableElement :: Config -> Parser Text
printableElement conf = msum
  [ lowers
  , uppers  conf
  , stop
  , newline
  , space
  , misc
  ] 

lowers :: Parser Text
lowers = fmap pack $ many1 lower >>= pass reset

uppers :: Config -> Parser Text
uppers conf = do
  text <- fmap pack $ many1 upper
  state <- getState
  if state == Skip
  then return text
  else do
    let (h,t) = uc text state
    pass reset $ intercalate (pack "") [h,replace' conf t]

stop :: Parser Text
stop = fmap singleton $ oneOf ".!?" >>= pass set

space :: Parser Text
space = fmap singleton $ P.space >>= pass sticky

newline :: Parser Text
newline = fmap singleton $ P.newline >>= pass inc

misc :: Parser Text
misc = fmap singleton $ anyChar >>= pass reset

pass :: Parser b -> a -> Parser a
pass m a = do
  state <- getState
  if state == Skip
  then return a
  else m >> return a

reset :: Parser ()
reset = setState None

set :: Parser ()
set = setState Stop

inc :: Parser ()
inc = modifyState inc' where
  inc' None = NewLine
  inc' _    = NewSentence

sticky :: Parser ()
sticky = modifyState inc' where
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
