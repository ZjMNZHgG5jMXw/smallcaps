module Main where

import System.Exit                    ( ExitCode )
import Data.Text                      ( pack )

import qualified Text.SmallCaps.TeX   as T ( TeX, TeXElement (..) )
import qualified Text.SmallCaps.LaTeX as L ( LaTeX, LaTeXElement (..) )
import Text.SmallCaps.TeXLaTeXParser       ( parse, latex )

import Tests                          ( failOn )

main :: IO ExitCode
main = failOn $ failed checks

checks :: [(T.TeX, L.LaTeX)]
checks =
  [ ([], [])
  , ([T.Printable (pack "abc")], [L.Printable (pack "abc")])
  , ([T.Macro (pack "\\abc")], [L.Macro (pack "\\abc") []])
  , ([T.Macro (pack "\\abc"), T.Printable (pack "def")], [L.Macro (pack "\\abc") [], L.Printable (pack "def")])
  , ([T.Macro (pack "\\abc"), T.Macro (pack "\\def")], [L.Macro (pack "\\abc") [], L.Macro (pack "\\def") []])
  , ([T.Macro (pack "\\abc"), T.Block [T.Printable (pack "def")]], [L.Macro (pack "\\abc") [L.Block [L.Printable (pack "def")]]])
  , ([T.Macro (pack "\\abc"), T.Block [T.Printable (pack "def")], T.Block [T.Printable (pack "ghi")]],
     [L.Macro (pack "\\abc") [L.Block [L.Printable (pack "def")], L.Block [L.Printable (pack "ghi")]]])
  , ([T.Macro (pack "\\abc"), T.Comment (pack "%def\n")], [L.Macro (pack "\\abc") [], L.Comment (pack "%def\n")])
  , ([T.Macro (pack "\\begin"), T.Block [T.Printable (pack "document")], T.Printable (pack "abc"), T.Macro (pack "\\end"), T.Block [T.Printable (pack "document")]],
     [L.Environment (pack "document") [L.Printable (pack "abc")]])
  , ([T.Block [T.Printable (pack "abc")]], [L.Block [L.Printable (pack "abc")]])
  , ([T.Comment (pack "%abc\n")], [L.Comment (pack "%abc\n")])
  ]

failed :: [(T.TeX, L.LaTeX)] -> [String]
failed = map (show . fst) . filter (uncurry ((/=) . fst . parse latex))

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
