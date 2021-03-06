module Main where

import System.Exit              ( ExitCode )
import Data.Text                ( pack )
import Data.Default             ( def )

import Text.SmallCaps.LaTeX          ( LaTeX, LaTeXElement (..) )
import Text.SmallCaps.Config         ( Config (..), defaultProfile, whitelist, after, isolateWith, clean )
import Text.SmallCaps.DocumentParser ( runDocument )

import Tests                    ( failOn )

type Check = ([LaTeXElement], (Config, [LaTeXElement]))

main :: IO ExitCode
main = failOn $ failed checks

checks :: [Check]
checks =
  [ noChange    testconf { search = after [] }              (\a b -> [a,b])
  , isChanging  testconf                                    (\a b -> [a,b])
  , isChanging  testconf                                    (\a b -> [Macro (pack "\\fun") [unchanged], a, b])
  , isChanging  testconf { search = whitelist ["\\fun"] }   (\a b -> [Macro (pack "\\fun") [a], b])
  , isChanging  testconf                                    (\a b -> [Environment (pack "fun") [unchanged], a, b])
  , isChanging  testconf { search = whitelist ["fun"] }     (\a b -> [Environment (pack "fun") [a], b])
  , isChanging  testconf                                    (\a b -> [Block [a], b])
  , isChanging  testconf                                    (\a b -> [Comment (pack "%AB\n"), a, b])
  , isChanging  testconf { isolate = iso [] }               (\a b -> [Macro (pack "\\fun") [Block [unchanged]], a, b])
  , isChanging  testconf { isolate = iso ["\\fun"] }        (\a b -> [Macro (pack "\\fun") [Block [a]], a, b])
  , isChanging  testconf { isolate = iso [] }               (\a b -> [Environment (pack "fun") [unchanged], a, b])
  , isChanging  testconf { isolate = iso ["fun"] }          (\a b -> [Environment (pack "fun") [a, b], a, b])
  , isChanging  testconf { isolate = iso [] }               (\a b -> [Block [a], b])
  , isChanging  testconf { isolate = iso [] }               (\a b -> [Comment (pack "%AB\n"), a, b])
  , isChanging  testconf { skip = after [] }                (\a b -> [Macro (pack "\\fun") [Block [unchanged]], a, b])
  , noChange    testconf { skip = after ["\\fun"] }         (\a b -> [Macro (pack "\\fun") [Block [unchanged]], a, b])
  , isChanging  testconf { skip = after [] }                (\a b -> [Environment (pack "fun") [unchanged], a, b])
  , noChange    testconf { skip = after ["fun"] }           (\a b -> [Environment (pack "fun") [unchanged], a, b])
  , isChanging  testconf { skip = after [] }                (\a b -> [Block [a], b])
  , isChanging  testconf { skip = after [] }                (\a b -> [Comment (pack "%AB\n"), a, b])
  , isChanging  testconf { skip = after ["\\fun"]
                         , unskip = after ["\\gun"] }       (\_ b -> [ Macro (pack "\\fun") [Block [unchanged]], unchanged
                                                                     , Macro (pack "\\gun") [Block [unchanged]], b, b ])
  , isChanging  testconf { skip = after ["fun"]
                         , unskip = after ["gun"] }         (\_ b -> [ Environment (pack "fun") [unchanged], unchanged
                                                                     , Environment (pack "gun") [unchanged], b, b ])
  , isChanging  testconf { eos = after [] }                 (\a b -> [a, Macro (pack "\\fun") [], b, b])
  , isChanging  testconf { eos = after ["\\fun"] }          (\a b -> [a, Macro (pack "\\fun") [], a, b])
  , isChanging  testconf { eos = after [] }                 (\a b -> [a, Environment (pack "fun") [], b, b])
  , isChanging  testconf { eos = after ["fun"] }            (\a b -> [a, Environment (pack "fun") [], a, b])
  ] where iso = isolateWith . map (flip (,) "default")

failed :: [Check] -> [String]
failed = map (show . fst) . filter (\(a, (conf, b)) -> not (either (const False) (== b) (runDocument conf defaultProfile a)))

isChanging, noChange :: Config -> (LaTeXElement -> LaTeXElement -> LaTeX) -> Check
isChanging  conf fun = (fun unchanged unchanged, (conf, fun changed changed'))
noChange    conf fun = (fun unchanged unchanged, (conf, fun unchanged unchanged))

unchanged, changed, changed' :: LaTeXElement
unchanged = Printable (pack "AB")
changed   = Printable (pack "A{\\small B}")
changed'  = Printable (pack "{\\small AB}")

testconf :: Config
testconf = clean
  { periodChars   = periodChars   def
  , search        = whitelist     []
  , replace       = replace       def
  , replaceFilter = replaceFilter def
  }

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
