module Main where

import Data.Default           ( def )
import System.Environment     ( withProgName )
import SmallCaps              ( smallcaps )

main :: IO ()
main = withProgName "lesscase" $ smallcaps def

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
