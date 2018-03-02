module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM.HTML
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window
import DOM.Node.NonElementParentNode
import DOM.Node.Types (ElementId(..))
import DOM.Node.Element (setAttribute)
import Partial.Unsafe (unsafePartial)
import Data.Maybe
import Math ((%))
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Time (seconds)

curve :: State -> String
curve { v, e, c } =
  let
    v8 = show $ 8.0*c
    _v8 = "-"<>v8
    v16 = show $ 16.0*c
    _v16 = "-"<>v16
    x = show $ 16.0-6.0*e
    y = show $ 32.0 - 16.0*(v+0.5*e) + 8.0*c
  in
    "m "<>x<>","<>y<>" c 16,"<>_v16<>" 24,"<>_v16<>" 48,"<>_v8<>" 24,"<>v8<>" 32,"<>v8<>" 48,"<>_v8

type State = { v :: Number, c :: Number, h :: Number, e :: Number }

render :: State -> { lower :: String, mid :: String, curved :: String, rot :: String }
render { v, e, c, h } =
  let across y h = "m 16," <> show y <> " h " <> show h in
  { lower: across (96.0 - 16.0*v) (96.0*h)
  , mid: across (64.0 - 16.0*v) 96.0
  , curved: curve { v, e, c, h }
  , rot: "rotate(" <> show (-33.0*e) <> " 16 32)"
  }

cyclic :: Behavior Number
cyclic = seconds <#> \t -> (t*2.5) % 5.5

pos :: Number -> State
pos t
  | t < 1.0 = { v: 0.0, e: 0.0, c: t, h: t }
  | t < 3.0 = { v: 0.0, e: 0.0, c: 1.0, h: 1.0 }
  | t < 4.0 = { v: 2.0*(t-3.0), e: 4.0*(t-3.0), c: 1.0, h: 1.0 }
  | t < 5.0 = { v: 2.0, e: 2.0, c: 1.0, h: 1.0 }
  | otherwise = { v: 0.0, e: 0.0, c: 0.0, h: 0.0 }

main :: Eff _ Unit
main = do
  d <- window >>= document >>> map htmlDocumentToNonElementParentNode
  let get i = unsafePartial fromJust <$> getElementById i d
  lower <- get (ElementId "lower")
  mid <- get (ElementId "mid")
  curved <- get (ElementId "curved")
  let b = cyclic <#> render <<< pos
  _ <- animate b \scene -> do
    let set d e = setAttribute "d" d e
    set scene.lower lower
    set scene.mid mid
    set scene.curved curved
    setAttribute "transform" scene.rot curved
  pure unit
