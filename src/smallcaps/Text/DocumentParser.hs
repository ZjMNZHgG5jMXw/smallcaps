module Text.DocumentParser where

import            Text.Parsec           ( runParser, getState, modifyState, putState, many )
import            Data.Text             ( Text, empty, pack, unpack, intercalate )
import            Control.Monad         ( msum )
import            Data.Default          ( def )

import            Data.LaTeX            ( LaTeX, LaTeXElement (..) )
import qualified  Text.LaTeXParser as L ( Parser )
import            Text.LaTeXParser      ( anyPrintable, anyMacro, anyEnvironment, anyBlock, anyComment )
import            Data.Config           ( ParserState (..), Config (..), StopState (..), SubParser )
import            Text.PrintableParser  ( runPrintableWith )
import            Text.ConfigParser     ( reconfigure )

type Parser = L.Parser ParserState

runDocument :: Config -> LaTeX -> LaTeX
runDocument conf = fst . runDocumentWith (def { config = conf })

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
  if not (ignore state)
  then putState (state' { ignore = False }) -- unskip at the block end
  else putState  state'
  return x'

isolateSubDocument :: SubParser a -> a -> Parser a
isolateSubDocument fun x = do
  state <- getState
  return $ fst $ fun (state { stop = def }) x

decideSub :: LaTeXElement -> SubParser a -> a -> Parser a
decideSub element fun x = sub =<< fmap config getState where
  sub conf
    | isolate conf element  = isolateSubDocument fun x
    | search  conf element  = runSubDocument fun x
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
  x@(Printable text) <- anyPrintable
  implySkip x
  text' <- decideSub x runPrintableWith text
  implyEos x
  return $ Printable text'

macro :: Parser LaTeXElement
macro = do
  x@(Macro name latex) <- anyMacro
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Macro name latex'

environment :: Parser LaTeXElement
environment = do
  x@(Environment name latex) <- anyEnvironment
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Environment name latex'

block :: Parser LaTeXElement
block = do
  x@(Block latex) <- anyBlock
  implySkip x
  latex' <- decideSub x runDocumentWith latex
  implyEos x
  return $ Block latex'

comment :: Parser LaTeXElement
comment = do
  x <- anyComment
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
    | skip conf element   = modifyState (\state -> state { ignore = True })
    | unskip conf element = modifyState (\state -> state { ignore = False })
    | otherwise           = return ()

implyEos :: LaTeXElement -> Parser ()
implyEos element = do
  conf <- fmap config getState
  if eos conf element
  then modifyState (\state -> state { stop = def })
  else return ()

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
