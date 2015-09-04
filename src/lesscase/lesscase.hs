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
---- The command line interface for SmallCaps.
----
-------------------------------------------------------------------------------

module Main where

import Data.Default           ( def )
import System.Environment     ( withProgName )
import Text.SmallCaps         ( smallcaps )
import Text.SmallCaps.Config  ( defaultProfile )

main :: IO ()
main = withProgName "lesscase" $ smallcaps def defaultProfile

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
