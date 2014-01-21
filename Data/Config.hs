module Data.Config where

import Data.Default
import Data.Text    hiding ( map, replace )
import Data.LaTeX
import Data.StopState

data Config = Config
  { initState :: StopState              -- initial document parser state
  , search    :: LaTeXElement -> Bool   -- search block/macro/environment for caps
  , isolate   :: LaTeXElement -> Bool   -- open an isolated state for a block/macro/environment
  , skip      :: LaTeXElement -> Bool   -- skip searching for the rest of the block etc.
  , eos       :: LaTeXElement -> Bool   -- end of sentence, start with new one
  , replace   :: Text -> Text           -- formatting for small caps
  }

instance Default Config where
  def = Config
    { initState = def
    , search    = defaultSearch
    , isolate   = defaultIsolate
    , skip      = defaultSkip
    , eos       = defaultEos
    , replace   = defaultReplace
    }

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

-- most conservative configuration
conservative :: Config
conservative = Config
  { initState = Skip
  , search    = const False
  , isolate   = const False
  , skip      = const False
  , eos       = const False
  , replace   = id
  }

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
