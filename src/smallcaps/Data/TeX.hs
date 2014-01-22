module Data.TeX where

import Data.Text ( Text )

type TeX = [TeXElement]

data TeXElement
  = Printable Text
  | Macro Text
  | Block TeX
  | Comment Text
  deriving Show

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
