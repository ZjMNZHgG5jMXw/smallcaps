module Text.DocumentParser where

import            Text.Parsec     hiding  ( satisfy )
import            Data.Text               ( Text, empty, pack, unpack, intercalate )
import            Control.Monad
import            Data.Default

import            Data.LaTeX
import qualified  Text.LaTeXParser     as L
import            Data.Config
import            Data.StopState
import            Text.PrintableParser    ( runPrintableWith )

type Parser = L.Parser StopState

runDocument :: Config -> LaTeX -> LaTeX
runDocument conf = fst . runDocumentWith def (initState conf)

runDocumentWith :: Config -> SubParser LaTeX
runDocumentWith conf state = either (error . show) id . runParser (stateAnd (document conf)) state ""
  where stateAnd p = do
          a <- p
          s <- getState
          return (a,s)

runSubDocument :: SubParser a -> a -> Parser a 
runSubDocument fun x = do
  state <- getState
  let (x', state') = fun state x
  if state /= Skip && state' == Skip
  then putState None -- replace Skip with None at the block end
  else putState state'
  return x'

isolateSubDocument :: SubParser a -> a -> Parser a
isolateSubDocument fun = return . fst . fun def

decideSub :: Config -> LaTeXElement -> SubParser a -> a -> Parser a
decideSub conf element fun
  | isolate conf element  = isolateSubDocument fun
  | search  conf element  = runSubDocument fun -- TODO unnecessary when user state == Skip
  | otherwise             = return

document :: Config -> Parser LaTeX
document = many . documentElement

documentElement :: Config -> Parser LaTeXElement
documentElement conf = msum
  [ printable   conf
  , macro       conf
  , environment conf
  , block       conf
  , comment     conf
  ]

printable :: Config -> Parser LaTeXElement
printable conf = do
  x@(Printable text) <- L.anyPrintable
  implySkip conf x
  text' <- decideSub conf x (runPrintableWith conf) text
  implyEos conf x
  return $ Printable text'

macro :: Config -> Parser LaTeXElement
macro conf = do
  x@(Macro name latex) <- L.anyMacro
  implySkip conf x
  latex' <- decideSub conf x (runDocumentWith conf) latex
  implyEos conf x
  return $ Macro name latex'

environment :: Config -> Parser LaTeXElement
environment conf = do
  x@(Environment name latex) <- L.anyEnvironment
  implySkip conf x
  latex' <- decideSub conf x (runDocumentWith conf) latex
  implyEos conf x
  return $ Environment name latex'

block :: Config -> Parser LaTeXElement
block conf = do
  x@(Block latex) <- L.anyBlock
  implySkip conf x
  latex' <- decideSub conf x (runDocumentWith conf) latex
  implyEos conf x
  return $ Block latex'

comment :: Config -> Parser LaTeXElement
comment conf = do
  x <- L.anyComment
  implySkip conf x
  implyEos conf x
  return x

implySkip :: Config -> LaTeXElement -> Parser ()
implySkip conf element
  | skip conf element = putState Skip
  | otherwise         = return ()

implyEos :: Config -> LaTeXElement -> Parser ()
implyEos conf element = do
  state <- getState
  if state /= Skip && eos conf element
  then putState def
  else return ()

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
