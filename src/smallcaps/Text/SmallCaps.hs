-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- The main program as library.
--
-------------------------------------------------------------------------------

module Text.SmallCaps where

import System.IO                  ( Handle, hClose, openFile, openTempFile, IOMode (..), stdin, stdout )
import Data.Text.IO               ( hGetContents, hPutStr )
import System.FilePath            ( (</>), takeDirectory, takeBaseName, makeRelative, hasExtension, replaceExtension )
import System.Directory           ( renameFile, canonicalizePath, makeRelativeToCurrentDirectory, createDirectoryIfMissing )
import System.Environment         ( getProgName, getArgs )
import System.Exit                ( exitFailure )
import System.Console.GetOpt      ( OptDescr ( Option ), ArgDescr ( NoArg, ReqArg ), ArgOrder ( Permute ), getOpt, usageInfo )
import Data.Version               ( Version ( Version ), versionBranch, versionTags, showVersion )
import Data.Default               ( def )
import Data.Attoparsec.Text       ( parseOnly )
import Data.Text                  ( Text, pack, unpack )
import Data.Map                   ( Map )
import qualified Data.Map  as Map ( empty, member, insert, elems )
import Control.Monad              ( foldM )

import Text.SmallCaps.Config           ( Config (..), conservative, busy, clean )
import Text.SmallCaps.LaTeX            ( LaTeX, unlatex )
import Text.SmallCaps.TeXParser        ( tex )
import Text.SmallCaps.TeXLaTeXParser   ( parse, latex )
import Text.SmallCaps.DocumentParser   ( runDocument, runDocument' )
import Text.SmallCaps.ConfigParser     ( replaceMacro, searchList, isolateList, skipList, unskipList, eosList )
import qualified Text.SmallCaps.ConfigParser as ConfigParser ( Style ( .. ) )

-- ** Meta information

version :: Version
version = Version
  { versionBranch = [0,4,1,3]
  , versionTags   = []
  }

-- ** Pure functions

smallcapsNoRecursion :: Config -> Text -> Either String Text
smallcapsNoRecursion conf = fmap unlatex . (runDocument conf =<<) . fmap fst . parseLaTeX

parseLaTeX :: Text -> Either String (LaTeX, [Text])
parseLaTeX = fmap (parse latex) . parseOnly tex

-- ** Main program function

smallcaps :: Config -> IO ()
smallcaps conf = uncurry (runFlags conf) =<< opts =<< getArgs

smallcapsHandle :: Handle -> Handle -> Config -> IO ()
smallcapsHandle inp out conf = hPutStr out =<< runParser =<< hGetContents inp
  where runParser = either fail return . smallcapsNoRecursion conf

smallcapsPipe :: Config -> IO ()
smallcapsPipe = smallcapsHandle stdin stdout

smallcapsFile :: FilePath -> FilePath -> Config -> IO ()
smallcapsFile pre inpFN conf = either fail (prefixed pre inpFN) . smallcapsNoRecursion conf =<< hGetContents =<< openFile inpFN ReadMode

smallcapsRecursiveFile :: FilePath -> FilePath -> Config -> IO ()
smallcapsRecursiveFile pre inpFN conf = do
  inp <- openFile inpFN ReadMode
  (inpCode, fs) <- either fail return =<< fmap parseLaTeX (hGetContents inp)
  ls <- recursiveContents (takeDirectory inpFN) Map.empty (map unpack fs)
  (outCode, ls') <- either fail return $ runDocument' ls conf inpCode
  prefixed pre inpFN $ unlatex outCode
  mapM_ (uncurry ((. unlatex) . prefixed pre)) $ Map.elems ls'

recursiveContents :: FilePath -> Map FilePath (FilePath, LaTeX) -> [FilePath] -> IO (Map FilePath (FilePath, LaTeX))
recursiveContents orig = foldM get where
  get ls path =
    if path `Map.member` ls
    then return ls
    else do
      fn <- canonicalizePath (orig </> makeRelative orig (texPath path))
      fh <- openFile fn ReadMode
      (code, fs) <- either fail return =<< fmap parseLaTeX (hGetContents fh)
      recursiveContents orig (Map.insert path (fn, code) ls) (map unpack fs)

prefixed :: FilePath -> FilePath -> Text -> IO ()
prefixed pre inpFN text =
  if pre == "" || pre == "."
  then do
    (outFN, out) <- openTempFile (takeDirectory inpFN) (takeBaseName inpFN)
    hPutStr out text
    hClose out
    renameFile outFN inpFN
  else do
    outFN <- fmap ((</>) pre) (makeRelativeToCurrentDirectory inpFN)
    createDirectoryIfMissing True (takeDirectory outFN)
    out <- openFile outFN WriteMode
    hPutStr out text
    hClose out

texPath :: FilePath -> FilePath
texPath path
  | hasExtension path = path
  | otherwise         = replaceExtension path ".tex"

getPrefix :: [Flag] -> String
getPrefix = foldr get "" where
  get (Prefix pre)  _    = pre
  get _             pre' = pre'

-- ** Program flags

data Flag
  = ProgVer
  | Help
  | Prefix    String
  | Recursive
  | Profile   String
  | Periods   String
  | BMacro    String
  | AMacro    String
  | Search    String
  | Isolate   String
  | Skip      String
  | Unskip    String
  | Eos       String
  | NoInline
  deriving (Eq, Show)

runFlags :: Config -> [Flag] -> [String] -> IO ()
runFlags conf flags filenames
  | Help `elem` flags       = usage
  | ProgVer `elem` flags    = putVersion
  | null filenames          = smallcapsPipe                                               (reconf conf flags)
  | Recursive `elem` flags  = smallcapsRecursiveFile  (getPrefix flags) (head filenames)  (reconf conf flags)
  | otherwise               = smallcapsFile           (getPrefix flags) (head filenames)  (reconf conf flags)

reconf :: Config -> [Flag] -> Config
reconf = foldl fun where
  fun _    (Profile s)  = chooseConf s
  fun conf (Periods s)  = conf { periodChars = s }
  fun conf (BMacro s)   = parse' (flip replaceMacro ConfigParser.NoArg) conf s
  fun conf (AMacro s)   = parse' (flip replaceMacro ConfigParser.InArg) conf s
  fun conf (Search s)   = parse' searchList conf s
  fun conf (Isolate s)  = parse' isolateList conf s
  fun conf (Skip s)     = parse' skipList conf s
  fun conf (Unskip s)   = parse' unskipList conf s
  fun conf (Eos s)      = parse' eosList conf s
  fun conf NoInline     = conf { inlineConfig = False }
  fun conf _            = conf
  chooseConf s
    | s == "conservative" = conservative
    | s == "busy"         = busy
    | s == "clean"        = clean
    | otherwise           = def
  parse' p conf = either (const conf) id . parseOnly (p conf) . pack

options :: [OptDescr Flag]
options =
  [ Option []     ["version"]   (NoArg ProgVer)             "version number"
  , Option ['h']  ["help"]      (NoArg Help)                "program usage"
  , Option []     ["prefix"]    (ReqArg Prefix  "<path>")   "output file prefix"
  , Option ['r']  ["recursive"] (NoArg Recursive)           "follow \\include and \\input statements"
  , Option []     ["no-inline"] (NoArg NoInline)            "ignore configuration statements in TeX comments"
  , Option ['p']  ["profile"]   (ReqArg Profile "<name>")   "configuration preset (conservative, busy, clean)"
  , Option ['x']  ["periods"]   (ReqArg Periods "<chars>")  "signs marking the end of a sentence (default: \".!?\")"
  , Option ['m']  ["macro"]     (ReqArg BMacro  "<code>")   "block macro that transforms small caps (default: \"\\small\")"
  , Option ['M']  ["macro-arg"] (ReqArg AMacro  "<code>")   "argument macro that transforms small caps (default: not set)"
  , Option ['s']  ["search"]    (ReqArg Search  "<list>")   "search list (default: \"+ document\")"
  , Option ['i']  ["isolate"]   (ReqArg Isolate "<list>")   "isolate list (default: \"+ \\footnote, \\marginpar\")"
  , Option ['S']  ["skip"]      (ReqArg Skip    "<list>")   "skip list (default: \"+ \\small\")"
  , Option ['u']  ["unskip"]    (ReqArg Skip    "<list>")   "unskip list (default: \"+ \\tiny, \\large, ...\")"
  , Option ['e']  ["eos"]       (ReqArg Eos     "<list>")   "end-of-sentence list (default: \"+ \\par, \\section, ...\")"
  ]

opts :: [String] -> IO ([Flag], [String])
opts argv = do
  case getOpt Permute options argv of
    (o,n,[])  -> return (o,n)
    (_,_,_ )  -> usage >> exitFailure

-- ** Help texts

usage :: IO ()
usage = do
  putVersion >> putStrLn ""
  pname     <- getProgName
  putStrLn  $ "Usage: " ++ pname ++ " [options] [filename]\n"
  putStr      "Options:"
  putStr    $ usageInfo "" options
  putStrLn    "\nWithout filename, the program starts in filter mode.\n"
  putStrLn    "Inline configuration:"
  putStrLn    " % smallcaps reset profile <name>"
  putStrLn    " % smallcaps restore profile <name>"
  putStrLn    "    reset configuration profile to <name>"
  putStrLn    "    default:       default configuration"
  putStrLn    "    conservative:  restrictive settings for few actions only"
  putStrLn    "    busy:          search all environments and macros"
  putStrLn    "    clean:         suppress all actions"
  putStrLn    ""
  putStrLn    " % smallcaps store profile <name>"
  putStrLn    "    store a configuration profile as <name>"
  putStrLn    ""
  putStrLn    " % smallcaps periods are <chars>"
  putStrLn    "    reset the end-of-sentence markers to <char>"
  putStrLn    "    only punctuation chars are accepted"
  putStrLn    ""
  putStrLn    " % smallcaps substitution <mode> <macro>"
  putStrLn    "    reset the macro to be used to mark small caps"
  putStrLn    "    <mode>:"
  putStrLn    "      in block with:   {\\small ABC}"
  putStrLn    "      as argument of:  \\caps{ABC}"
  putStrLn    ""
  putStrLn    " % smallcaps search + <list>"
  putStrLn    "    add macros or environments to the search list"
  putStrLn    " % smallcaps search - <list>"
  putStrLn    "    remove macros or environments to the search list"
  putStrLn    " % smallcaps search *"
  putStrLn    "    all macros and environments are searched"
  putStrLn    " % smallcaps search /"
  putStrLn    "    no macros nor environments are searched"
  putStrLn    ""
  putStrLn    " % smallcaps isolate <options as in search>" 
  putStrLn    "    isolate the parser state within the macro arguments"
  putStrLn    "    or environment bodies"
  putStrLn    "    write a profile name before '+' or '*' in order to"
  putStrLn    "    use a specific profile for the isolation."
  putStrLn    " % smallcaps skip <options as in search>"
  putStrLn    "    skip the following contents until the macro argument"
  putStrLn    "    or the environment ends"
  putStrLn    " % smallcaps unskip <options as in search>"
  putStrLn    "    undo skip"
  putStrLn    " % smallcaps eos <options as in search>"
  putStrLn    "    starts a new sentence after the macro or environemt"
  putStrLn    ""
  putStrLn    " <list> is a comma-separated list of macro names including"
  putStrLn    " backslash ('\\') and environment names.\n"

putVersion :: IO ()
putVersion = putStrLn =<< fmap (++ " version " ++ showVersion version) getProgName

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
