module Main where

import System.Exit                        ( ExitCode )
import Data.Text                          ( cons, pack, append )
import qualified Data.Default as Default  ( def )

import Data.LaTeX                         ( LaTeXElement (..) )
import Data.Config                        ( ParserState (..), Config (..), clean )
import Text.ConfigParser                  ( reconfigure )

import Tests                              ( failOn )

main :: IO ExitCode
main = failOn $ failed checks

checks :: [(String, Config -> Bool)]
checks =
  [ ("% smallcaps reset profile default abc\n",               checkDefault)
  , ("% SMALLCAPS RESET PROFILE default abc\n",               checkDefault)
  , ("% smallcaps restore profile default abc\n",             checkDefault)
  , ("% SMALLCAPS RESTORE PROFILE default abc\n",             checkDefault)
  , ("% smallcaps periods are .! abc\n",                      checkBlackWhitePeriods "?" ".!")
  , ("% SMALLCAPS PERIODS ARE .! abc\n",                      checkBlackWhitePeriods "?" ".!")
  , ("% smallcaps substitution in block with \\small abc\n",  checkSubstBlock "\\small")
  , ("% SMALLCAPS SUBSTITUTION IN BLOCK WITH \\small abc\n",  checkSubstBlock "\\small")
  , ("% smallcaps substitution as argument of \\small abc\n", checkSubstArg "\\small")
  , ("% SMALLCAPS SUBSTITUTION AS ARGUMENT OF \\small abc\n", checkSubstArg "\\small")
  , ("% smallcaps search + abc, \\def ghi\n",   checkBlackWhite search  [ghi] [abc, def, mno, pqr])
  , ("% SMALLCAPS SEARCH + abc, \\def ghi\n",   checkBlackWhite search  [ghi] [abc, def, mno, pqr])
  , ("% smallcaps search - \\mno jkl\n",        checkBlackWhite search  [abc, def, ghi, jkl, mno] [pqr])
  , ("% SMALLCAPS SEARCH - \\mno jkl\n",        checkBlackWhite search  [abc, def, ghi, jkl, mno] [pqr])
  , ("% smallcaps search * abc\n",              checkBlackWhite search  [] [abc, def, ghi, jkl, mno, pqr])
  , ("% SMALLCAPS SEARCH * abc\n",              checkBlackWhite search  [] [abc, def, ghi, jkl, mno, pqr])
  , ("% smallcaps search / abc\n",              checkBlackWhite search  [abc, def, ghi, jkl, mno, pqr] [])
  , ("% SMALLCAPS SEARCH / abc\n",              checkBlackWhite search  [abc, def, ghi, jkl, mno, pqr] [])
  {-
  , ("% smallcaps isolate + abc, \\def ghi\n",  checkBlackWhite isolate [ghi, jkl] [abc, def, mno, pqr])
  , ("% SMALLCAPS ISOLATE + abc, \\def ghi\n",  checkBlackWhite isolate [ghi, jkl] [abc, def, mno, pqr])
  , ("% smallcaps isolate - \\mno jkl\n",       checkBlackWhite isolate [abc, def, ghi, jkl, mno] [pqr])
  , ("% SMALLCAPS ISOLATE - \\mno jkl\n",       checkBlackWhite isolate [abc, def, ghi, jkl, mno] [pqr])
  , ("% smallcaps isolate * abc\n",             checkBlackWhite isolate [] [abc, def, ghi, jkl, mno, pqr])
  , ("% SMALLCAPS ISOLATE * abc\n",             checkBlackWhite isolate [] [abc, def, ghi, jkl, mno, pqr])
  , ("% smallcaps isolate / abc\n",             checkBlackWhite isolate [abc, def, ghi, jkl, mno, pqr] [])
  , ("% SMALLCAPS ISOLATE / abc\n",             checkBlackWhite isolate [abc, def, ghi, jkl, mno, pqr] [])
  -}
  , ("% smallcaps skip + abc, \\def ghi\n",     checkBlackWhite skip    [ghi, jkl] [abc, def, mno, pqr])
  , ("% SMALLCAPS SKIP + abc, \\def ghi\n",     checkBlackWhite skip    [ghi, jkl] [abc, def, mno, pqr])
  , ("% smallcaps skip - \\mno jkl\n",          checkBlackWhite skip    [abc, def, ghi, jkl, mno] [pqr])
  , ("% SMALLCAPS SKIP - \\mno jkl\n",          checkBlackWhite skip    [abc, def, ghi, jkl, mno] [pqr])
  , ("% smallcaps skip * abc\n",                checkBlackWhite skip    [] [abc, def, ghi, jkl, mno, pqr])
  , ("% SMALLCAPS SKIP * abc\n",                checkBlackWhite skip    [] [abc, def, ghi, jkl, mno, pqr])
  , ("% smallcaps skip / abc\n",                checkBlackWhite skip    [abc, def, ghi, jkl, mno, pqr] [])
  , ("% SMALLCAPS SKIP / abc\n",                checkBlackWhite skip    [abc, def, ghi, jkl, mno, pqr] [])
  , ("% smallcaps unskip + abc, \\def ghi\n",   checkBlackWhite unskip  [ghi, jkl] [abc, def, mno, pqr])
  , ("% SMALLCAPS UNSKIP + abc, \\def ghi\n",   checkBlackWhite unskip  [ghi, jkl] [abc, def, mno, pqr])
  , ("% smallcaps unskip - \\mno jkl\n",        checkBlackWhite unskip  [abc, def, ghi, jkl, mno] [pqr])
  , ("% SMALLCAPS UNSKIP - \\mno jkl\n",        checkBlackWhite unskip  [abc, def, ghi, jkl, mno] [pqr])
  , ("% smallcaps unskip * abc\n",              checkBlackWhite unskip  [] [abc, def, ghi, jkl, mno, pqr])
  , ("% SMALLCAPS UNSKIP * abc\n",              checkBlackWhite unskip  [] [abc, def, ghi, jkl, mno, pqr])
  , ("% smallcaps unskip / abc\n",              checkBlackWhite unskip  [abc, def, ghi, jkl, mno, pqr] [])
  , ("% SMALLCAPS UNSKIP / abc\n",              checkBlackWhite unskip  [abc, def, ghi, jkl, mno, pqr] [])
  , ("% smallcaps eos + abc, \\def ghi\n",      checkBlackWhite eos     [ghi, jkl] [abc, def, mno, pqr])
  , ("% SMALLCAPS EOS + abc, \\def ghi\n",      checkBlackWhite eos     [ghi, jkl] [abc, def, mno, pqr])
  , ("% smallcaps eos - \\mno jkl\n",           checkBlackWhite eos     [abc, def, ghi, jkl, mno] [pqr])
  , ("% SMALLCAPS EOS - \\mno jkl\n",           checkBlackWhite eos     [abc, def, ghi, jkl, mno] [pqr])
  , ("% smallcaps eos * abc\n",                 checkBlackWhite eos     [] [abc, def, ghi, jkl, mno, pqr])
  , ("% SMALLCAPS EOS * abc\n",                 checkBlackWhite eos     [] [abc, def, ghi, jkl, mno, pqr])
  , ("% smallcaps eos / abc\n",                 checkBlackWhite eos     [abc, def, ghi, jkl, mno, pqr] [])
  , ("% SMALLCAPS EOS / abc\n",                 checkBlackWhite eos     [abc, def, ghi, jkl, mno, pqr] [])
  ]

failed :: [(String, Config -> Bool)] -> [String]
failed = map (filter (/='\n') . fst) . filter (\(a,b) -> either (const True) (not . b) $ reconfigure teststate (pack a))

checkDefault :: Config -> Bool
checkDefault conf
  =   checkBlackWhitePeriods ",:" ".!?" conf
  &&  checkSubstBlock "\\small" conf
  &&  checkBlackWhite search [Macro (pack "\\fun") []] [Environment (pack "document") [], Macro (pack "\\\\") []] conf
  -- &&  checkBlackWhite isolate [Macro (pack "\\fun") []] [Macro (pack "\\footnote") [], Macro (pack "\\marginpar") []] conf
  &&  checkBlackWhite skip [Macro (pack "\\normalsize") []]
        [ Macro (pack "\\tiny") []
        , Macro (pack "\\scriptsize") []
        , Macro (pack "\\footnotesize") []
        , Macro (pack "\\small") []
        , Macro (pack "\\large") []
        , Macro (pack "\\Large") []
        , Macro (pack "\\LARGE") []
        , Macro (pack "\\huge") []
        , Macro (pack "\\Huge") []
        ] conf
  &&  checkBlackWhite unskip
        [ Macro (pack "\\tiny") []
        , Macro (pack "\\scriptsize") []
        , Macro (pack "\\footnotesize") []
        , Macro (pack "\\small") []
        , Macro (pack "\\large") []
        , Macro (pack "\\Large") []
        , Macro (pack "\\LARGE") []
        , Macro (pack "\\huge") []
        , Macro (pack "\\Huge") []
        ] [Macro (pack "\\normalsize") []] conf
  &&  checkBlackWhite eos [Macro (pack "\\fun") []]
        [ Macro (pack "\\par") []
        , Macro (pack "\\part") []
        , Macro (pack "\\chapter") []
        , Macro (pack "\\section") []
        , Macro (pack "\\subsection") []
        , Macro (pack "\\subsubsection") []
        , Macro (pack "\\paragraph") []
        , Environment (pack "itemize") []
        , Environment (pack "enumerate") []
        , Environment (pack "description") []
        ] conf

checkBlackWhitePeriods :: [Char] -> [Char] -> Config -> Bool
checkBlackWhitePeriods blacks whites conf = foldr ((&&) . (`elem` periodChars conf)) (not (foldr ((||) . (`elem` periodChars conf)) False blacks)) whites

checkSubstBlock :: String -> Config -> Bool
checkSubstBlock macro conf = replace conf (pack "cba") == cons '{' (pack macro) `append` (pack " cba}")

checkSubstArg :: String -> Config -> Bool
checkSubstArg macro conf = replace conf (pack "cba") == pack macro `append` pack "{cba}"

checkBlackWhite :: (Config -> LaTeXElement -> Bool) -> [LaTeXElement] -> [LaTeXElement] -> Config -> Bool
checkBlackWhite fun blacks whites conf = foldr ((&&) . fun conf) (not (foldr ((||) . fun conf) False blacks)) whites

teststate :: ParserState
teststate = Default.def { config = testconf }

testconf :: Config
testconf = clean
  { search  = def'
  -- , isolate = def'
  , skip    = def'
  , unskip  = def'
  , eos     = def'
  } where
    def' (Macro name [])        = name == (pack "\\mno")
    def' (Environment name [])  = name == (pack "pqr")
    def' _                      = False

abc :: LaTeXElement
abc = Environment (pack "abc") []

def :: LaTeXElement
def = Macro (pack "\\def") []

ghi :: LaTeXElement
ghi = Environment (pack "ghi") []

jkl :: LaTeXElement
jkl = Environment (pack "jkl") []

mno :: LaTeXElement
mno = Macro (pack "\\mno") []

pqr :: LaTeXElement
pqr = Environment (pack "pqr") []

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
