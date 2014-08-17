-------------------------------------------------------------------------------
-- |
-- Module      :  Text.SmallCaps.Config
-- Copyright   :  (c) Stefan Berthold 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  stefan.berthold@gmx.net
-- Stability   :  unstable
-- Portability :  GHC
--
-- This module specifies the default configuration values for SmallCaps.
--
-------------------------------------------------------------------------------

module Text.SmallCaps.Config where

import            Data.Default    ( Default, def )
import            Data.Text       ( Text, pack, snoc, append )
import            Data.Map        ( Map )
import qualified  Data.Map as Map ( empty, fromList )
import            Control.Monad   ( liftM2 )

import            Text.SmallCaps.LaTeX  ( LaTeX, LaTeXElement
                                        , isPrintable, isMacro, isEnvironment, isBlock, isBBlock, isMath, isComment
                                        , name
                                        )

-- ** Parser user state

data ParserState = ParserState
  { config  :: Config                           -- ^ configuration
  , inputs  :: Map FilePath (FilePath, LaTeX)   -- ^ additional input files
  , profile :: Profile                          -- ^ configuration preset list
  , stop    :: StopState                        -- ^ stop state
  , ignore  :: Bool                             -- ^ skip on/off
  }

instance Default ParserState where
  def = ParserState
    { config  = def
    , inputs  = Map.empty
    , profile = defaultProfile
    , stop    = def
    , ignore  = False
    }

defaultProfile :: Map Text Config
defaultProfile = Map.fromList [ (pack "default",      def)
                              , (pack "clean",        clean)
                              , (pack "conservative", conservative)
                              , (pack "busy",         busy)
                              , (pack "small",        small)
                              ]

-- ** Configuration data type

data Config = Config
  { periodChars   :: [Char]                     -- ^ signs recognised as periods
  , search        :: LaTeXElement -> Bool       -- ^ search block/macro/environment for caps
  , isolate       :: LaTeXElement -> Maybe Text -- ^ open an isolated state for a block/macro/environment; returns config name
  , skip          :: LaTeXElement -> Bool       -- ^ skip searching for the rest of the block etc.
  , unskip        :: LaTeXElement -> Bool       -- ^ undo skip, e.g., at @\normalsize@ when skipping @\small@
  , eos           :: LaTeXElement -> Bool       -- ^ end of sentence, start with new one
  , replace       :: Text -> Text               -- ^ formatting for small caps
  , exceptions    :: [PatternReplace]           -- ^ search for patterns in printables and replace them (no further processing)
  , inlineConfig  :: Bool                       -- ^ dynamic reconfiguration in LaTeX comments
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
    , exceptions    = defaultExceptions
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
  , "\\part*", "\\chapter*", "\\section*", "\\subsection*", "\\subsubsection*"
  , "\\include"
  , "itemize", "enumerate", "description"
  ]

defaultReplace :: Text -> Text
defaultReplace caps = pack "{\\small " `append` snoc caps '}'

defaultExceptions :: [PatternReplace]
defaultExceptions = []

-- ** Configuration presets

-- | combinator for plugin construction
(&&&) :: (LaTeXElement -> Bool) -> (LaTeXElement -> Bool) -> LaTeXElement -> Bool
(&&&) fun gun element = fun element && gun element

(|||) :: (LaTeXElement -> Bool) -> (LaTeXElement -> Bool) -> LaTeXElement -> Bool
(|||) fun gun element = fun element || gun element

-- | clean configuration, all substitutions off
clean :: Config
clean = Config
  { periodChars   = []
  , search        = const False
  , isolate       = isolateWith []
  , skip          = const False
  , unskip        = const False
  , eos           = const False
  , replace       = id
  , exceptions    = []
  , inlineConfig  = True
  }

-- | conservative configuration
conservative :: Config
conservative = def
  { search        = whitelist []
  , isolate       = isolateWith []
  , eos           = after ["\\par"]
  }

-- | busy configuration
busy :: Config
busy = conservative { search = blacklist [] }

-- | abstract/small font configuration
small :: Config
small = def
  { skip    = (not . after ["\\small"])       &&& (after ["\\normalsize"] ||| defaultSkip)
  , unskip  = (not . after ["\\normalsize"])  &&& (after ["\\small"]      ||| defaultUnskip)
  , replace = \caps -> pack "{\\footnotesize " `append` snoc caps '}'
  }

whitelist :: [String] -> LaTeXElement -> Bool
whitelist names = liftM2 (||) (liftM2 (||) (liftM2 (||) isBlock isBBlock) isPrintable) (after names)

blacklist :: [String] -> LaTeXElement -> Bool
blacklist names = not . liftM2 (||) (liftM2 (||) isMath isComment) (after names)

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

-- ** Profile data type

type Profile = Map Text Config

-- ** Subparser data type

type SubParser a = ParserState -> a -> Either String (a, ParserState)

-- ** Stop state

data StopState
  = None          -- ^ within a sentence
  | NewLine       -- ^ one newline read
  | Stop          -- ^ stop character read
  | NewSentence   -- ^ begin of a new sentence
  deriving (Show, Eq)

instance Default StopState where
  def = NewSentence

-- ** Pattern search and replace (exceptions from processing)

data PatternReplace = PatternReplace
  { pattern     :: Text
  , replacement :: Text
  }

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
