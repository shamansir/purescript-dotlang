module Data.DotLang.Attr.Global where

import Prelude

import Data.DotLang.Class (class DotLang, toText)

data RankDirValue
  = FromTop
  | FromLeft
  | FromBottom
  | FromRight

instance rankDirValueDotLang :: DotLang RankDirValue where
  toText FromTop = "TB"
  toText FromLeft = "LR"
  toText FromBottom = "BT"
  toText FromRight = "RL"

-- | Upper-case first character is major order;
-- | lower-case second character is minor order.
data PageDirValue = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt

instance pageDirValueDotLang :: DotLang PageDirValue where
  toText Bl = "BL"
  toText Br = "BR"
  toText Tl = "TL"
  toText Tr = "TR"
  toText Rb = "RB"
  toText Rt = "RT"
  toText Lb = "LB"
  toText Lt = "LT"

data LabelJustValue = L | R

instance DotLang LabelJustValue where
  toText L = "l"
  toText R = "r"

data LabelLocValue = T | B

instance DotLang LabelLocValue where
  toText T = "t"
  toText B = "b"

data StyleValue = Filled | Striped | Rounded

instance DotLang StyleValue where
  toText Filled = "filled"
  toText Striped = "striped"
  toText Rounded = "rounded"

data Attr
  = RankDir RankDirValue
  | PageDir PageDirValue
  | Label String
  | LabelJust LabelJustValue
  | LabelLoc LabelLocValue
  | Compound Boolean
  | Style StyleValue

instance attrDotLang :: DotLang Attr where
  toText (RankDir dir) = "rankdir=" <> toText dir
  toText (PageDir dir) = "pagedir=" <> toText dir
  toText (Label val) = "label=" <> show val
  toText (LabelJust val) = "labeljust=" <> toText val
  toText (LabelLoc val) = "labelloc=" <> toText val
  toText (Compound val) = "compound=" <> show val
  toText (Style val) = "style=" <> toText val
