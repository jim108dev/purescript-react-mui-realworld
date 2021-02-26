{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Paper where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data AriaHaspopup :: Type

ariaHaspopup ::
  { dialog :: AriaHaspopup
  , "false" :: AriaHaspopup
  , grid :: AriaHaspopup
  , listbox :: AriaHaspopup
  , menu :: AriaHaspopup
  , tree :: AriaHaspopup
  , "true" :: AriaHaspopup
  }
ariaHaspopup = { dialog: unsafeCoerce "dialog", "false": unsafeCoerce "false", grid: unsafeCoerce "grid", listbox: unsafeCoerce "listbox", menu: unsafeCoerce "menu", tree: unsafeCoerce "tree", "true": unsafeCoerce "true" }

instance eqAriaHaspopup :: Eq AriaHaspopup where
  eq = unsafeRefEq

type PaperClassesGenericRow a
  = ( elevation0 :: a
    , elevation1 :: a
    , elevation10 :: a
    , elevation11 :: a
    , elevation12 :: a
    , elevation13 :: a
    , elevation14 :: a
    , elevation15 :: a
    , elevation16 :: a
    , elevation17 :: a
    , elevation18 :: a
    , elevation19 :: a
    , elevation2 :: a
    , elevation20 :: a
    , elevation21 :: a
    , elevation22 :: a
    , elevation23 :: a
    , elevation24 :: a
    , elevation3 :: a
    , elevation4 :: a
    , elevation5 :: a
    , elevation6 :: a
    , elevation7 :: a
    , elevation8 :: a
    , elevation9 :: a
    , outlined :: a
    , root :: a
    , rounded :: a
    )

type PaperClassesKey
  = PaperClassesGenericRow String

type PaperClassesJSS
  = PaperClassesGenericRow JSS

type PaperOptPropsRow (r :: # Type)
  = ( "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , children :: Array JSX
    , classes :: { | PaperClassesKey }
    , elevation :: Number
    , square :: Boolean
    | r
    )

type PaperReqPropsRow (r :: # Type)
  = r

type PaperPropsRow (r :: # Type)
  = PaperOptPropsRow (PaperReqPropsRow r)

foreign import _UnsafePaper :: forall componentProps. ReactComponent { | PaperPropsRow componentProps }

_Paper ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaperReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaperPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Paper = unsafeCoerce _UnsafePaper

paper ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaperReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaperPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
paper ps = element _Paper ps

paper' :: PaperProps -> JSX
paper' = MUI.React.Basic.element _Paper'

_Paper' :: ReactComponent PaperProps
_Paper' = unsafeCoerce _UnsafePaper

paperWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ PaperClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (PaperProps -> JSX)
paperWithStyles style = render
  where
  withStyles' :: ReactComponent PaperProps -> Effect.Effect (ReactComponent PaperProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Paper'

  render = map MUI.React.Basic.element styledComponent

foreign import data PaperProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (PaperReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (PaperPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> PaperProps
props = unsafeCoerce