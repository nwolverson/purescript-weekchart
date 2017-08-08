module Chart where

import Prelude
import CSS (backgroundColor, color, display, div, fromString, height, inlineBlock, margin, paddingLeft, px, rgb, steelblue, width) as CSS
import CSS.VerticalAlign (textBottom, verticalAlign) as CSS
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import CSS ((?))
import Data.Foldable (maximum)
import Data.Formatter.Number (formatOrShowNumber)
import Data.Maybe (fromMaybe)
import Data.Traversable (sum)
import Halogen (ComponentHTML)
import Halogen.HTML.CSS (style, stylesheet)

type Range = { min:: Number, max:: Number }
type Scale = Number -> Number

linearScale :: Range -> Range -> Scale
linearScale { min, max } { min: rangeMin, max: rangeMax } x =
  (x - min) / (max - min) * (rangeMax - rangeMin) + rangeMin
  where
    x' = case x of
      _ | x < min -> min
      _ | x > max -> max
      _ | otherwise -> x

data F a = F a

renderDays :: Array Number -> ComponentHTML F
renderDays input =
  H.div_ $ [
      stylesheet do
        CSS.div ? CSS.fromString ".bar" ? do
          CSS.backgroundColor CSS.steelblue
          CSS.margin (CSS.px 1.0) (CSS.px 1.0) (CSS.px 1.0) (CSS.px 1.0)
          CSS.display CSS.inlineBlock
    ] <>
    map (\n -> H.div [
      P.class_ (H.ClassName "bar"),
      style do
        CSS.width (CSS.px 8.0)
        CSS.height (CSS.px $ ysc n)
      ] [ H.text "" ]) input <>
    [ H.span [
        style do
          CSS.color (CSS.rgb 120 120 120)
          CSS.paddingLeft (CSS.px 5.0)
          CSS.verticalAlign CSS.textBottom
      ]
      [ H.text $ formatOrShowNumber "00.0" ((sum input) /1000.0) <> "km" ]
    ]

  where

  ysc = linearScale { min: 0.0, max: fromMaybe 0.0 $ maximum input } { min: 0.0, max: 30.0 }
