module Text.DocumentParser where

import            Text.Parsec     hiding  ( satisfy )
import            Data.Text               ( Text, empty, pack, unpack, intercalate )
import            Control.Monad
import            Data.Default

import            Data.LaTeX
import qualified  Text.LaTeXParser     as L
import            Data.Config
import            Text.PrintableParser    ( runPrintableWith )
import            Text.ConfigParser       ( reconfigure )

type Parser = L.Parser ParserState

runDocument :: Config -> LaTeX -> LaTeX
runDocument conf = fst . runDocumentWith (ParserState { config = conf, stop = initState conf })

runDocumentWith :: SubParser LaTeX
runDocumentWith state = either (error . show) id . runParser (stateAnd document) state ""
  where stateAnd p = do
          a <- p
          s <- getState
          return (a,s)

runSubDocument :: SubParser a -> a -> Parser a 
runSubDocument fun x = do
  state <- getState
  let (x', state') = fun state x
  if stop state /= Skip && stop state' == Skip
  then modifyState (\state -> state { stop = None }) -- replace Skip with None at the block end
  else putState state'
  return x'

isolateSubDocument :: SubParser a -> a -> Parser a
isolateSubDocument fun x = do
  state <- getState
  return $ fst $ fun (state { stop = initState (config state) }) x

decideSub :: LaTeXElement -> SubParser a -> a -> Parser a
decideSub element fun x = sub =<< fmap config getState where
  sub conf
    | isolate conf element  = isolateSubDocument fun x
    | search  conf element  = runSubDocument fun x -- TODO unnecessary when user state == Skip
    | otherwise             = return x

document :: Parser LaTeX
document = many documentElement

documentElement :: Parser LaTeXElement
documentElement = msum
  [ printable
  , macro
  , environment
  , block
  , comment
  ]

printable :: Parser LaTeXElement
printable = do
  x@(Printable text) <- L.anyPrintable
  implySkip x
  text' <- decideSub x runPrintableWith text
  implyEos x
  return $ Printable text'

macro :: Parser LaTeXElement
macro = do
  x@(Macro name latex) <- L.anyMacro
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Macro name latex'

environment :: Parser LaTeXElement
environment = do
  x@(Environment name latex) <- L.anyEnvironment
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Environment name latex'

block :: Parser LaTeXElement
block = do
  x@(Block latex) <- L.anyBlock
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Block latex'

comment :: Parser LaTeXElement
comment = do
  x <- L.anyComment
  implySkip x
  implyEos x
  let (Comment text) = x
  conf <- fmap config getState
  if inlineConfig conf
  then maybe (return ()) (\c -> modifyState (\s -> s { config = c })) $ reconfigure conf text
  else return ()
  return x

implySkip :: LaTeXElement -> Parser ()
implySkip element = sub =<< fmap config getState where
  sub conf
    | skip conf element = modifyState (\state -> state { stop = Skip })
    | otherwise         = return ()

implyEos :: LaTeXElement -> Parser ()
implyEos element = do
  state <- getState
  if stop state /= Skip && eos (config state) element
  then modifyState (\state -> state { stop = initState (config state) })
  else return ()

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
