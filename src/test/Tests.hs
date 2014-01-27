module Tests where

import System.Exit ( ExitCode, exitSuccess, exitFailure )

failOn :: [String] -> IO ExitCode
failOn fs
  | fs == []  = exitSuccess
  | otherwise = putStrLn ('\n':es) >> exitFailure
  where es = unlines $ map ((++) "* FAILED: ") fs

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
