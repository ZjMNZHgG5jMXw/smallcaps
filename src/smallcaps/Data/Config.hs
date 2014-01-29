module Data.Config where

import            Data.Default    ( Default, def )
import            Data.Text       ( Text, pack, snoc, append )
import            Data.Map        ( Map )
import qualified  Data.Map as Map ( fromList )
import            Control.Monad   ( liftM2 )

import            Data.LaTeX      ( LaTeXElement
                                  , isPrintable, isMacro, isEnvironment, isBlock, isComment
                                  , name
                                  )

data StopState
  = None
  | NewLine
  | Stop
  | NewSentence
  deriving (Show, Eq)

instance Default StopState where
  def = NewSentence

type SubParser a = ParserState -> a -> Either String (a, ParserState)

data ParserState = ParserState
  { config  :: Config
  , profile :: Map Text Config
  , stop    :: StopState
  , ignore  :: Bool
  }

instance Default ParserState where
  def = ParserState
    { config  = def
    , profile = Map.fromList  [ (pack "default",      def)
                              , (pack "clean",        clean)
                              , (pack "conservative", conservative)
                              , (pack "busy",         busy)
                              , (pack "small",        small)
                              ]
    , stop    = def
    , ignore  = False
    }

data Config = Config
  { periodChars   :: [Char]                     -- signs recognised as periods
  , search        :: LaTeXElement -> Bool       -- search block/macro/environment for caps
  , isolate       :: LaTeXElement -> Maybe Text -- open an isolated state for a block/macro/environment; returns config name
  , skip          :: LaTeXElement -> Bool       -- skip searching for the rest of the block etc.
  , unskip        :: LaTeXElement -> Bool       -- undo skip, e.g., at \normalsize when skipping \small
  , eos           :: LaTeXElement -> Bool       -- end of sentence, start with new one
  , replace       :: Text -> Text               -- formatting for small caps
  , inlineConfig  :: Bool                       -- dynamic reconfiguration in LaTeX comments
  }

instance Default Config where
  def = Config
    { periodChars   = defaultPeriodChars
    , search        = defaultSearch
    , isolate       = defaultIsolate
    , skip          = defaultSkip
    , unskip        = defaultUnskip
    , eos           = defaultEos
    , replace       = defaultReplace
    , inlineConfig  = True
    }

defaultPeriodChars :: [Char]
defaultPeriodChars = ".!?"

defaultSearch :: LaTeXElement -> Bool
defaultSearch = whitelist ["document", "\\\\"]

defaultIsolate :: LaTeXElement -> Maybe Text
defaultIsolate = isolateWith  [ ("abstract",    "small")
                              , ("\\footnote",  "small")
                              , ("\\marginpar", "default")
                              ]

defaultSkip :: LaTeXElement -> Bool
defaultSkip = after [ "\\tiny", "\\scriptsize", "\\footnotesize", "\\small"
                    , "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge"]

defaultUnskip :: LaTeXElement -> Bool
defaultUnskip = after ["\\normalsize"]

defaultEos :: LaTeXElement -> Bool
defaultEos = after
  [ "\\par"
  , "\\part", "\\chapter", "\\section", "\\subsection", "\\subsubsection", "\\paragraph"
  , "itemize", "enumerate", "description"
  ]

defaultReplace :: Text -> Text
defaultReplace caps = pack "{\\small " `append` snoc caps '}'

-- combinator for plugin construction
(&&&) :: (LaTeXElement -> Bool) -> (LaTeXElement -> Bool) -> LaTeXElement -> Bool
(&&&) fun gun element = fun element && gun element

(|||) :: (LaTeXElement -> Bool) -> (LaTeXElement -> Bool) -> LaTeXElement -> Bool
(|||) fun gun element = fun element || gun element

-- clean configuration, all substitutions off
clean :: Config
clean = Config
  { periodChars   = []
  , search        = const False
  , isolate       = isolateWith []
  , skip          = const False
  , unskip        = const False
  , eos           = const False
  , replace       = id
  , inlineConfig  = True
  }

-- conservative configuration
conservative :: Config
conservative = def
  { search        = whitelist []
  , isolate       = isolateWith []
  , eos           = after ["\\par"]
  }

-- busy configuration
busy :: Config
busy = conservative { search = blacklist [] }

-- abstract/small font configuration
small :: Config
small = def
  { skip    = (not . after ["\\small"])       &&& (after ["\\normalsize"] ||| defaultSkip)
  , unskip  = (not . after ["\\normalsize"])  &&& (after ["\\small"]      ||| defaultUnskip)
  , replace = \caps -> pack "{\\footnotesize " `append` snoc caps '}'
  }

whitelist :: [String] -> LaTeXElement -> Bool
whitelist names = liftM2 (||) (liftM2 (||) isPrintable isBlock) (after names)

blacklist :: [String] -> LaTeXElement -> Bool
blacklist names = not . liftM2 (||) isComment (after names)

after :: [String] -> LaTeXElement -> Bool
after names = liftM2 (&&) (liftM2 (||) isMacro isEnvironment) (flip elem (map pack names) . name)

isolateWith :: [(String, String)] -> LaTeXElement -> Maybe Text
isolateWith names x
  | isMacro x || isEnvironment x  = findConfigName (name x) names
  | otherwise                     = Nothing

findConfigName :: Text -> [(String, String)] -> Maybe Text
findConfigName name' = foldr fun Nothing
  where fun (n,c) Nothing | pack n == name' = Just (pack c)
                          | otherwise       = Nothing
        fun _     x                         = x

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
