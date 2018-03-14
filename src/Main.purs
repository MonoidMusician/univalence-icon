module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import DOM (DOM)
import DOM.Classy.Node (appendChild)
import DOM.Event.EventTarget as EL
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), Element)
import Data.Array ((..))
import Data.Foldable (foldMap, for_)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.String (trim)
import FRP.Behavior (Behavior, animate)
import FRP.Behavior.Time (seconds)
import Math ((%))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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
  let across y h = "m 16," <> show y <> " h " <> show h in
  { lower: across (96.0 - 16.0*v) (96.0*h)
  , mid: across (64.0 - 16.0*v) 96.0
  , curved: curve { v, e, c, h }
  , rot: "rotate(" <> show (-33.0*e) <> " 16 32)"
  }

rollover = 5.5 :: Number

cyclic :: Behavior Number
cyclic = seconds <#> \t -> (t*2.5) % rollover

pos :: Number -> State
pos t
  | t < 1.0 = { v: 0.0, e: 0.0, c: t, h: t }
  | t < 2.0 = { v: 0.0, e: 0.0, c: 1.0, h: 1.0 }
  | t < 2.5 = { v: 0.0, e: 1.0*(t-2.0)*(t-2.0), c: 1.0, h: 1.0 }
  | t < 3.5 = { v: 2.0*(t-2.5), e: 1.0*(t-2.0)*(t-2.0), c: 1.0, h: 1.0 }
  | t < 4.0 = { v: 2.0, e: 2.0, c: 1.0, h: 1.0 }
  | otherwise = { v: 0.0, e: 0.0, c: 0.0, h: 0.0 }

foreign import canvg :: forall e. EffFn2 ( dom :: DOM | e ) Element String Unit
foreign import saveCanvas :: forall e. EffFn2 ( dom :: DOM | e ) Element String Unit

main :: Eff _ Unit
main = do
  w <- window
  d <- document w <#> htmlDocumentToNonElementParentNode
  let get i = unsafePartial fromJust <$> getElementById i d
  lower <- get (ElementId "lower")
  mid <- get (ElementId "mid")
  curved <- get (ElementId "curved")
  let
    render scene = do
      let set d e = setAttribute "d" d e
      set scene.lower lower
      set scene.mid mid
      set scene.curved curved
      setAttribute "transform" scene.rot curved
    renderAt = pos >>> infos >>> render
  canceller <- animate cyclic renderAt
  let astep = 1.0/8.0
  step <- Ref.newRef (-2.0 * astep)
  let
    listener = EL.eventListener \_ -> do
      s0 <- Ref.readRef step
      when (s0 < 0.0) canceller
      Ref.modifyRef step $ (_ + astep) >>> (_ % rollover)
      s <- Ref.readRef step
      renderAt s
      logShow (s / astep)
    cclick = EL.eventListener \_ -> do
      let forFrame frame = infos $ pos $ frame / 8.0
      for_ (0..44) \frame -> do
        let
          blur = 15
          blurred = 1.0 / toNumber blur
          allpaths = foldMap blurFrame (1..blur)
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
            </svg>
            """
        el <- createElement "canvas" <<< htmlDocumentToDocument =<< document w
        setAttribute "width" "128" el
        setAttribute "height" "128" el
        _ <- appendChild el <<< unsafePartial fromJust =<< body =<< document w
        runEffFn2 canvg el (trim svg)
        runEffFn2 saveCanvas el ("frame" <> show frame)
  EL.addEventListener (EventType "click") listener false (unsafeCoerce w)
  EL.addEventListener (EventType "dblclick") cclick false (unsafeCoerce w)
  pure unit
