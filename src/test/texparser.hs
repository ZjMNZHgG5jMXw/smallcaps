module Main where

import System.Exit          ( ExitCode )
import Data.Text            ( pack )
import Data.Attoparsec.Text ( parseOnly )

import Data.TeX             ( TeX, TeXElement (..) )
import Text.TeXParser       ( tex )

import Tests                ( failOn )

main :: IO ExitCode
main = failOn $ failed checks

checks :: [(String, TeX)]
checks =
  [ ("", [])
  --
  , ("abc",       [Printable (pack "abc")])
  , ("\\abc",     [Macro (pack "\\abc")])
  , ("%abc",      [Comment (pack "%abc\n")])
  , ("%abc\n",    [Comment (pack "%abc\n")])
  , ("{}",        [Block []])
  , ("{abc}",     [Block [Printable (pack "abc")]])
  , ("{\\abc}",   [Block [Macro (pack "\\abc")]])
  , ("{%abc\n}",  [Block [Comment (pack "%abc\n")]])
  --
  , ("abc\\def",    [Printable (pack "abc"), Macro (pack "\\def")])
  , ("abc{}",       [Printable (pack "abc"), Block []])
  , ("abc%def",     [Printable (pack "abc"), Comment (pack "%def\n")])
  , ("\\abc123",    [Macro (pack "\\abc"), Printable (pack "123")])
  , ("\\abc def",   [Macro (pack "\\abc"), Printable (pack " def")])
  , ("\\abc\\def",  [Macro (pack "\\abc"), Macro (pack "\\def")])
  , ("\\abc{}",     [Macro (pack "\\abc"), Block []])
  , ("\\abc%def",   [Macro (pack "\\abc"), Comment (pack "%def\n")])
  , ("{}abc",       [Block [], Printable (pack "abc")])
  , ("{}\\abc",     [Block [], Macro (pack "\\abc")])
  , ("{}{}",        [Block [], Block []])
  , ("{}%abc",      [Block [], Comment (pack "%abc\n")])
  , ("%\nabc",      [Comment (pack "%\n"), Printable (pack "abc")])
  , ("%abc\ndef",   [Comment (pack "%abc\n"), Printable (pack "def")])
  , ("%\n\\abc",    [Comment (pack "%\n"), Macro (pack "\\abc")])
  , ("%\n{}",       [Comment (pack "%\n"), Block []])
  , ("%\n%abc",     [Comment (pack "%\n"), Comment (pack "%abc\n")])
  ]

failed :: [(String, TeX)] -> [String]
failed = map fst . filter (uncurry ((. Right) . (/=) . parseOnly tex . pack))

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
