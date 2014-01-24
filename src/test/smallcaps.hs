module Main where

import System.Exit  ( ExitCode, exitSuccess, exitFailure )
import Data.Text    ( pack )
import Data.Default ( def )
import SmallCaps

checks :: [(String, String)]
checks =
  [ ("", "")
  , ("abc", "abc")
  , ("ABC", "A{\\small BC}")
  , ("abcABC", "abc{\\small ABC}")
  , ("\\small ABC\\normalsize ABC", "\\small ABC\\normalsize {\\small ABC}")
  ]

main :: IO ExitCode
main  | fs == []  = exitSuccess
      | otherwise = putStrLn ('\n':es) >> exitFailure
      where fs = failed checks
            es = unlines $ map (\(a,b) -> "* FAILED: " ++ a ++ " => " ++ b) fs

failed :: [(String, String)] -> [(String, String)]
failed = filter (\(a,b) -> smallcaps def (pack a) /= Right (pack b))

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
