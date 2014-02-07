module Main where

import System.Exit                ( ExitCode )
import Data.Text                  ( Text, pack )
import Data.Default               ( def )

import Text.SmallCaps.Config          ( ParserState (..), StopState (..) )
import Text.SmallCaps.PrintableParser ( runPrintableWith )

import Tests                      ( failOn )

main :: IO ExitCode
main = failOn $ failed checks

checks :: [(String, (ParserState, (Text, ParserState) -> Bool))]
checks =
  [ ("a",       (def { stop = None },         checkOutStop "a" None))
  , ("a",       (def { stop = NewLine },      checkOutStop "a" None))
  , ("a",       (def { stop = Stop },         checkOutStop "a" None))
  , ("a",       (def { stop = NewSentence },  checkOutStop "a" None))
  , ("AB",      (def { stop = None },         checkOutStop "{\\small AB}" None))
  , ("AB",      (def { stop = NewLine },      checkOutStop "{\\small AB}" None))
  , ("AB",      (def { stop = Stop },         checkOutStop "{\\small AB}" None))
  , ("AB",      (def { stop = NewSentence },  checkOutStop "A{\\small B}" None))
  , (" ",       (def { stop = None },         checkOutStop " " None))
  , (" ",       (def { stop = NewLine },      checkOutStop " " NewLine))
  , (" ",       (def { stop = Stop },         checkOutStop " " NewSentence))
  , (" ",       (def { stop = NewSentence },  checkOutStop " " NewSentence))
  , (".",       (def { stop = None },         checkOutStop "." Stop))
  , (".",       (def { stop = NewLine },      checkOutStop "." Stop))
  , (".",       (def { stop = Stop },         checkOutStop "." Stop))
  , (".",       (def { stop = NewSentence },  checkOutStop "." Stop))
  , ("\n",      (def { stop = None },         checkOutStop "\n" NewLine))
  , ("\n",      (def { stop = NewLine },      checkOutStop "\n" NewSentence))
  , ("\n",      (def { stop = Stop },         checkOutStop "\n" NewSentence))
  , ("\n",      (def { stop = NewSentence },  checkOutStop "\n" NewSentence))
  ]

failed :: [(String, (ParserState, (Text, ParserState) -> Bool))] -> [String]
failed = map fst . filter (\(s, (state, fun)) -> not (either (const False) fun (runPrintableWith state (pack s))))

checkOutStop :: String -> StopState -> (Text, ParserState) -> Bool
checkOutStop s stopState (t, parserState) = (pack s) == t && stopState == stop parserState

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
