module Text.DocumentParser where

import            Text.Parsec           ( runParser, getState, modifyState, putState, many )
import            Control.Monad         ( msum )
import            Data.Default          ( def )
import qualified  Data.Map       as Map ( insert, lookup )

import            Data.LaTeX            ( LaTeX, LaTeXElement (..), name, body, content )
import qualified  Text.LaTeXParser as L ( Parser )
import            Text.LaTeXParser      ( anyPrintable, anyMacro, anyEnvironment, anyBlock, anyComment )
import            Data.Config           ( ParserState (..), Config (..), SubParser )
import            Text.PrintableParser  ( runPrintableWith )
import            Text.ConfigParser     ( reconfigure )

type Parser = L.Parser ParserState

runDocument :: Config -> LaTeX -> Either String LaTeX
runDocument conf = either Left (Right . fst) . runDocumentWith (def { config = conf })

runDocumentWith :: SubParser LaTeX
runDocumentWith state = either (Left . show) Right . runParser (stateAnd document) state ""
  where stateAnd p = do
          a <- p
          s <- getState
          return (a,s)

runSubDocument :: SubParser a -> a -> Parser a 
runSubDocument fun x = do
  state <- getState
  (x', state') <- either fail return (fun state x)
  if not (ignore state)
  then putState (state' { ignore = False }) -- unskip at the block end
  else putState  state'
  return x'

isolateSubDocument :: Config -> SubParser a -> a -> Parser a
isolateSubDocument conf fun x = do
  state <- getState
  either fail (return . fst) $ fun (state { config = conf, stop = def }) x

decideSub :: LaTeXElement -> SubParser a -> a -> Parser a
decideSub element fun x = do
  state <- getState
  let conf = config state
  maybe
    ( if search conf element then runSubDocument fun x else return x )
    ( flip (flip isolateSubDocument fun) x )
    $ maybe Nothing (flip Map.lookup (profile state))
    $ isolate conf element

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
  x <- anyPrintable
  implySkip x
  text <- decideSub x runPrintableWith (content x)
  implyEos x
  return $ Printable text

macro :: Parser LaTeXElement
macro = do
  x <- anyMacro
  implySkip x
  latex <- decideSub x runDocumentWith (body x)
  implyEos x
  return $ Macro (name x) latex

environment :: Parser LaTeXElement
environment = do
  x <- anyEnvironment
  implySkip x
  latex <- decideSub x runDocumentWith (body x)
  implyEos x
  return $ Environment (name x) latex

block :: Parser LaTeXElement
block = do
  x <- anyBlock
  implySkip x
  latex <- decideSub x runDocumentWith (body x)
  implyEos x
  return $ Block latex

comment :: Parser LaTeXElement
comment = do
  x <- anyComment
  implySkip x
  implyEos x
  state <- getState
  if inlineConfig (config state)
  then either
    (\(n,c) -> modifyState (\s -> s { profile = Map.insert n c (profile s) }))
    (\c     -> modifyState (\s -> s { config = c })) $ reconfigure state (content x)
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
