-------------------------------------------------------------------------------
---- |
---- Module      :  Main
---- Copyright   :  (c) Stefan Berthold 2014-2015
---- License     :  BSD3-style (see LICENSE)
----
---- Maintainer  :  stefan.berthold@gmx.net
---- Stability   :  unstable
---- Portability :  
----
---- Alternative implementation for the command line interface of SmallCaps.
----
-------------------------------------------------------------------------------

module Main where

import Data.Default           ( def )
import Data.Map               ( insert )
import System.Environment     ( withProgName )
import Data.Text as T         ( Text, pack, length )
import Text.SmallCaps         ( smallcaps )
import Text.SmallCaps.Config  ( Config ( replaceFilter ), Profile, defaultProfile, small, footnote )

main :: IO ()
main  = withProgName "twocase"
      $ smallcaps twoConf twoProfile

twoFilter :: Text -> Bool
twoFilter = (<) 1 . T.length

twoConf :: Config
twoConf = def { replaceFilter = twoFilter }

twoSmall :: Config
twoSmall = small { replaceFilter = twoFilter }

twoFootnote :: Config
twoFootnote = footnote { replaceFilter = twoFilter }

twoProfile :: Profile
twoProfile  = insert (pack "default")   twoConf
            $ insert (pack "small")     twoSmall
            $ insert (pack "footnote")  twoFootnote
            $ defaultProfile

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
