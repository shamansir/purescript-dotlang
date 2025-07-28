module Data.DotLang.Attr where

import Prelude

import Data.DotLang.Class (class DotLang)
data FillStyle
  = Filled
  | Dotted
  | Invis

instance fillStyleDotLang :: DotLang FillStyle where
  toText Filled = "filled"
  toText Dotted = "dotted"
  toText Invis = "invis"
