module Data.Config where

import Data.Default     ( Default, def )
import Data.Text        ( Text, pack, snoc, append )
import Data.LaTeX       ( LaTeXElement (..) )

data StopState
  = None
  | NewLine
  | Stop
  | NewSentence
  deriving (Show, Eq)

instance Default StopState where
  def = NewSentence

type SubParser a = ParserState -> a -> (a, ParserState)

data ParserState = ParserState
  { config  :: Config
  , stop    :: StopState
  , ignore  :: Bool       -- prev. Skip :: StopState
  }

data Config = Config  -- TODO remove initState
  { initState     :: StopState              -- initial document parser state
  , periodChars   :: [Char]                 -- signs recognised as periods
  , search        :: LaTeXElement -> Bool   -- search block/macro/environment for caps
  , isolate       :: LaTeXElement -> Bool   -- open an isolated state for a block/macro/environment
  , skip          :: LaTeXElement -> Bool   -- skip searching for the rest of the block etc.
  , unskip        :: LaTeXElement -> Bool   -- undo skip, e.g., at \normalsize when skipping \small
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
    , unskip        = defaultUnskip
    , eos           = defaultEos
    , replace       = defaultReplace
    , inlineConfig  = True
    }

defaultPeriodChars :: [Char]
defaultPeriodChars = ".!?"

defaultSearch :: LaTeXElement -> Bool
defaultSearch = whitelist ["document", "\\\\"]

defaultIsolate :: LaTeXElement -> Bool
defaultIsolate = after ["\\footnote", "\\marginpar"]

defaultSkip :: LaTeXElement -> Bool
defaultSkip = after [ "\\tiny", "\\scriptsize", "\\footnotesize", "\\small"
                    , "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge"]

defaultUnskip :: LaTeXElement -> Bool
defaultUnskip = after ["\\normalsize"]

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
  { initState     = None
  , periodChars   = []
  , search        = const False
  , isolate       = const False
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
  , isolate       = const False
  , eos           = after ["\\par"]
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
