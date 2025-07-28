module Data.DotLang.Attr.Edge where

import Prelude
import Color (Color, toHexString)
import Data.DotLang.Attr (FillStyle)
import Data.DotLang.Class (class DotLang, toText)

data LabelValue
  = TextLabel String
  | HtmlLabel String

data ArrowHeadStyle
  = Normal
  | Inv
  | Dot
  | InvDot
  | ODot
  | InvODot
  | None
  | Tee
  | Empty
  | InvEmpty
  | Diamond
  | ODiamond
  | EDiamond
  | Crow
  | Box
  | OBox
  | Open
  | HalfOpen
  | Vee

instance arrowHeadStyle :: DotLang ArrowHeadStyle where
  toText Normal = "normal"
  toText Inv = "inv"
  toText Dot = "dot"
  toText InvDot = "invdot"
  toText ODot = "odot"
  toText InvODot = "invodot"
  toText None = "none"
  toText Tee = "tee"
  toText Empty = "empty"
  toText InvEmpty = "invempty"
  toText Diamond = "diamond"
  toText ODiamond = "odiamond"
  toText EDiamond = "ediamond"
  toText Crow = "crow"
  toText Box = "box"
  toText OBox = "obox"
  toText Open = "open"
  toText HalfOpen = "halfopen"
  toText Vee = "vee"

data Attr
  = Color Color
  | FontColor Color
  | FontSize Int
  | Label LabelValue
  | Style FillStyle
  | FillColor Color
  | PenWidth Number
  | ArrowHead ArrowHeadStyle

instance attrDotLang :: DotLang Attr where
  toText (Color s) = "color=\"" <> toHexString s <> "\""
  toText (FontColor s) = "fontcolor=\"" <> toHexString s <> "\""
  toText (FontSize i) = "fontsize=" <> show i
  toText (Style f) = "style=" <> toText f
  toText (Label (TextLabel t)) = "label=" <> show t
  toText (Label (HtmlLabel t)) = "label=" <> t
  toText (FillColor c) = "fillcolor=\"" <> toHexString c <> "\""
  toText (PenWidth i) = "penwidth=" <> show i
  toText (ArrowHead s) = "arrowhead=" <> toText s

-- |
--| ```purescript run
--| > import Data.DotLang.Attr.Edge
--| > :t htmlLabel "<table><tr><td>Label</td></tr></table>"
--| Attr
--| ```
-- | htmlLabel as a part of an attribute of an edge.
htmlLabel :: String -> Attr
htmlLabel = HtmlLabel >>> Label

-- |
--| ```purescript run
--| > :t label "..."
--| Attr
--| ```
-- | label as a part of an attribute of an edge.
label :: String -> Attr
label = TextLabel >>> Label
