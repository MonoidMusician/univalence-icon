module Main where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldMap, for_)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Time (seconds)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element (setAttribute)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget as EL
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

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

infos :: State -> { lower :: String, mid :: String, curved :: String, rot :: String }
infos { v, e, c, h } =
  let across y w = "m 16," <> show y <> " h " <> show w in
  { lower: across (96.0 - 16.0*v) (96.0*h)
  , mid: across (64.0 - 16.0*v) 96.0
  , curved: curve { v, e, c, h }
  , rot: "rotate(" <> show (-33.0*e) <> " 16 32)"
  }

rollover = 5.5 :: Number

cyclic :: Behavior Number
cyclic = seconds <#> unwrap >>> \t -> (t*2.5) % rollover

pos :: Number -> State
pos t
  | t < 1.0 = { v: 0.0, e: 0.0, c: t, h: t }
  | t < 2.0 = { v: 0.0, e: 0.0, c: 1.0, h: 1.0 }
  | t < 2.5 = { v: 0.0, e: 1.0*(t-2.0)*(t-2.0), c: 1.0, h: 1.0 }
  | t < 3.5 = { v: 2.0*(t-2.5), e: 1.0*(t-2.0)*(t-2.0), c: 1.0, h: 1.0 }
  | otherwise = { v: 2.0, e: 2.0, c: 1.0, h: 1.0 }

foreign import data Zip :: Type
foreign import canvg :: EffectFn2 Element String Unit
foreign import saveCanvas :: EffectFn3 Element String Zip Unit
foreign import mkZip :: Effect Zip
foreign import saveZip :: EffectFn1 Zip Unit

main :: Effect Unit
main = do
  w <- window
  d <- document w <#> HTMLDocument.toNonElementParentNode
  let get i = unsafePartial fromJust <$> getElementById i d
  lower <- get "lower"
  mid <- get "mid"
  curved <- get "curved"
  let
    render scene = do
      let set = setAttribute "d"
      set scene.lower lower
      set scene.mid mid
      set scene.curved curved
      setAttribute "transform" scene.rot curved
    renderAt = pos >>> infos >>> render
  canceller <- animate cyclic renderAt
  let astep = 1.0/8.0
  step <- Ref.new (-1.0 * astep)
  listener <- EL.eventListener \_ -> do
    s0 <- Ref.read step
    when (s0 < 0.0) canceller
    step # Ref.modify_ do (_ + astep) >>> (_ % rollover)
    s <- Ref.read step
    renderAt s
    -- logShow (s / astep)
  cclick <- EL.eventListener \_ -> do
    let forFrame frame = infos $ pos $ frame / 8.0
    zip <- mkZip
    for_ (0..44) \frame -> do
      let
        blur = 13
        blurred = 1.0 / toNumber blur
        allpaths = foldMap blurFrame (4..(blur-3))
        blurFrame f = paths (forFrame (toNumber frame + toNumber f * blurred))
        paths scene =
          """
          <path d=""" <> show scene.lower <> """></path>
          <path d=""" <> show scene.mid <> """></path>
          <path d=""" <> show scene.curved <> """ transform=""" <> show scene.rot <> """></path>
          """
        svg =
          """
          <svg>
            <g id="layer1" style="fill:none;stroke:#000000;stroke-width:8;stroke-linecap:butt;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:""" <> show 0.25 <> """">
            """ <> allpaths <> """
            </g>
            <g id="layer2" style="fill:none;stroke:#ffffff;stroke-width:20;stroke-linecap:square;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:""" <> show 0.25 <> """">
            """ <> allpaths <> """
            </g>
          </svg>
          """
      el <- createElement "canvas" <<< HTMLDocument.toDocument =<< document w
      setAttribute "width" "128" el
      setAttribute "height" "128" el
      _ <- appendChild (Element.toNode el) <<< HTMLElement.toNode <<< unsafePartial fromJust =<< body =<< document w
      runEffectFn2 canvg el svg
      runEffectFn3 saveCanvas el ("frame" <> show frame) zip
    runEffectFn1 saveZip zip
  EL.addEventListener (EventType "click") listener false (unsafeCoerce w)
  EL.addEventListener (EventType "dblclick") cclick false (unsafeCoerce w)
  pure unit
