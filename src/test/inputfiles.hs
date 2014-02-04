module Main where

import System.Exit                        ( ExitCode )
import Data.Text                          ( Text, pack )
import qualified Data.Set as Set          ( fromList )

import Data.TeX                           ( TeX, TeXElement (..) )
import Text.TeXLaTeXParser                ( parse, latex )

import Tests                              ( failOn )

main :: IO ExitCode
main = failOn $ failed checks

checks :: [(TeX, [Text] -> Bool)]
checks =
  [ ([fun],                                     checkWhite [])
  , ([input, fun],                              checkWhite ["fun"])
  , ([include, fun],                            checkWhite ["fun"])
  , ([input, fun, include, gun],                checkWhite ["fun", "gun"])
  , ([noinput, fun],                            checkWhite [])
  , ([input, fun, noinput, gun, include, sun],  checkWhite ["fun", "sun"])
  , ([Block [input, fun]],                      checkWhite ["fun"])
  , ([Comment (pack "fun")],                    checkWhite [])
  ]

failed :: [(TeX, [Text] -> Bool)] -> [String]
failed = map (show . fst) . filter (\(a,b) -> (not . b . snd) $ parse latex a)

checkWhite :: [String] -> [Text] -> Bool
checkWhite whites = (ws ==) . Set.fromList where
  ws = Set.fromList (map pack whites)

fun :: TeXElement
fun = Block [Printable (pack "fun")]

gun :: TeXElement
gun = Block [Printable (pack "gun")]

sun :: TeXElement
sun = Block [Printable (pack "sun")]

input :: TeXElement
input = Macro (pack "\\input")

noinput :: TeXElement
noinput = Macro (pack "\\noinput")

include :: TeXElement
include = Macro (pack "\\include")

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai

