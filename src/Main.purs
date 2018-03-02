module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Behavior as HB
import Halogen.HTML.Properties as HP
import FRP.Behavior (Behavior)
import FRP.Behavior.Time (seconds)
import Svg.Elements (svg, path)
import Svg.Attributes as SVG
import Math ((%))
import Unsafe.Coerce (unsafeCoerce)

type State = {}
type Slot = Unit
type SubQuery = HB.Query ( hover :: Boolean ) ( style :: Maybe String, d :: Maybe String ) Unit Void
data Query a
  = DoNothing a

component :: forall u v. H.Component HH.HTML Query u v (Aff _)
component = H.lifecycleParentComponent
  { initialState: const {}
  , initializer: Nothing
  , finalizer: Nothing
  , receiver: const Nothing
  , eval
  , render
  } where
    b :: { hover :: Behavior Boolean } -> { style :: Behavior (Maybe String), d :: Behavior (Maybe String) }
    b { hover } =
      { d: cycle <#> \t -> pure
          let
            v8 = show $ 8.0*t
            _v8 = "-"<>v8
            v16 = show $ 16.0*t
            _v16 = "-"<>v16
            s = show $ 32.0 + 8.0*t
          in
            "m 16,"<>s<>" c 16,"<>_v16<>" 24,"<>_v16<>" 48,"<>_v8<>" 24,"<>v8<>" 32,"<>v8<>" 48,"<>_v8
      , style: pure $ pure
          """"
          fill: none;
          stroke: #000000;
          stroke-width:8;
          stroke-linecap:butt;
          stroke-miterlimit:4;
          stroke-dasharray:none;
          stroke-opacity:1
          """
      }
    cycle = seconds <#> \t -> t % 6.0
    bc = (HB.behavioralComponent (\p _ -> path p) <@> b) \_ el _ ->
      el [HP.id_ "hi"] []
    plain c = HH.slot unit c unit absurd
    parent = svg [unsafeCoerce SVG.width 128.0, unsafeCoerce SVG.height 128.0]
    render :: State -> H.ParentHTML Query SubQuery Slot (Aff _)
    render _ = parent [plain bc]
    eval :: Query ~> H.ParentDSL State Query SubQuery Slot v (Aff _)
    eval (DoNothing a) = pure a

main :: Eff _ Unit
main = runHalogenAff $ awaitBody >>= runUI component unit
