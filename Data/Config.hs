module Data.Config where

import Data.Default
import Data.Text    hiding ( map, replace )
import Data.LaTeX

data StopState
  = None
  | NewLine
  | Stop
  | NewSentence
  | Skip
  deriving (Show, Eq)

instance Default StopState where
  def = NewSentence

type SubParser a = ParserState -> a -> (a, ParserState)

data ParserState = ParserState
  { config  :: Config
  , stop    :: StopState
  }

data Config = Config
  { initState     :: StopState              -- initial document parser state
  , periodChars   :: [Char]                 -- signs recognised as periods
  , search        :: LaTeXElement -> Bool   -- search block/macro/environment for caps
  , isolate       :: LaTeXElement -> Bool   -- open an isolated state for a block/macro/environment
  , skip          :: LaTeXElement -> Bool   -- skip searching for the rest of the block etc.
  , eos           :: LaTeXElement -> Bool   -- end of sentence, start with new one
  , replace       :: Text -> Text           -- formatting for small caps
  , inlineConfig  :: Bool                   -- dynamic reconfiguration in LaTeX comments
  }

instance Default Config where
  def = Config
    { initState     = def
    , periodChars   = defaultPeriodChars
    , search        = defaultSearch
    , isolate       = defaultIsolate
    , skip          = defaultSkip
    , eos           = defaultEos
    , replace       = defaultReplace
    , inlineConfig  = True
    }

defaultPeriodChars :: [Char]
defaultPeriodChars = ".!?"

defaultSearch :: LaTeXElement -> Bool
defaultSearch = whitelist ["document"]

defaultIsolate :: LaTeXElement -> Bool
defaultIsolate = after ["\\footnote", "\\marginpar"]

defaultSkip :: LaTeXElement -> Bool
defaultSkip = after ["\\small"]

defaultEos :: LaTeXElement -> Bool
defaultEos = after
  [ "\\par"
  , "\\part", "\\chapter", "\\section", "\\subsection", "\\subsubsection", "\\paragraph"
  , "itemize", "enumerate", "describe"
  ]

defaultReplace :: Text -> Text
defaultReplace caps = pack "{\\small " `append` snoc caps '}'

-- combinator for plugin construction
(|||) :: (LaTeXElement -> Bool) -> (LaTeXElement -> Bool) -> LaTeXElement -> Bool
(|||) fun gun element = fun element || gun element

-- clean configuration, all substitutions off
clean :: Config
clean = Config
  { initState     = Skip
  , periodChars   = []
  , search        = const False
  , isolate       = const False
  , skip          = const False
  , eos           = const False
  , replace       = id
  , inlineConfig  = True
  }

-- conservative configuration
conservative :: Config
conservative = Config
  { initState     = def
  , periodChars   = defaultPeriodChars
  , search        = whitelist []
  , isolate       = const False
  , skip          = defaultSkip
  , eos           = after ["\\par"]
  , replace       = defaultReplace
  , inlineConfig  = True
  }

-- busy configuration
busy :: Config
busy = conservative { search = blacklist [] }

whitelist :: [String] -> LaTeXElement -> Bool
whitelist _     (Printable _)     = True
whitelist _     (Block _)         = True
whitelist names latex             = after names latex

blacklist :: [String] -> LaTeXElement -> Bool
blacklist _     (Comment _)       = False
blacklist names latex             = not $ after names latex

after :: [String] -> LaTeXElement -> Bool
after names (Macro name _)        = name `elem` map pack names
after names (Environment name _)  = name `elem` map pack names
after _ _                         = False

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
