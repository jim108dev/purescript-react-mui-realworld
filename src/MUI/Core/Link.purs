{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Link where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_a) as React.Basic.DOM
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

foreign import data Color :: Type

color ::
  { error :: Color
  , inherit :: Color
  , initial :: Color
  , primary :: Color
  , secondary :: Color
  , textPrimary :: Color
  , textSecondary :: Color
  }
color = { error: unsafeCoerce "error", inherit: unsafeCoerce "inherit", initial: unsafeCoerce "initial", primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary", textPrimary: unsafeCoerce "textPrimary", textSecondary: unsafeCoerce "textSecondary" }

foreign import data Underline :: Type

underline ::
  { always :: Underline
  , hover :: Underline
  , none :: Underline
  }
underline = { always: unsafeCoerce "always", hover: unsafeCoerce "hover", none: unsafeCoerce "none" }

foreign import data Variant :: Type

variant ::
  { body1 :: Variant
  , body2 :: Variant
  , button :: Variant
  , caption :: Variant
  , h1 :: Variant
  , h2 :: Variant
  , h3 :: Variant
  , h4 :: Variant
  , h5 :: Variant
  , h6 :: Variant
  , inherit :: Variant
  , overline :: Variant
  , srOnly :: Variant
  , subtitle1 :: Variant
  , subtitle2 :: Variant
  }
variant = { body1: unsafeCoerce "body1", body2: unsafeCoerce "body2", button: unsafeCoerce "button", caption: unsafeCoerce "caption", h1: unsafeCoerce "h1", h2: unsafeCoerce "h2", h3: unsafeCoerce "h3", h4: unsafeCoerce "h4", h5: unsafeCoerce "h5", h6: unsafeCoerce "h6", inherit: unsafeCoerce "inherit", overline: unsafeCoerce "overline", srOnly: unsafeCoerce "srOnly", subtitle1: unsafeCoerce "subtitle1", subtitle2: unsafeCoerce "subtitle2" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqUnderline :: Eq Underline where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

instance eqAriaHaspopup :: Eq AriaHaspopup where
  eq = unsafeRefEq

type LinkClassesGenericRow a
  = ( alignCenter :: a
    , alignJustify :: a
    , alignLeft :: a
    , alignRight :: a
    , body1 :: a
    , body2 :: a
    , button :: a
    , caption :: a
    , colorError :: a
    , colorInherit :: a
    , colorPrimary :: a
    , colorSecondary :: a
    , colorTextPrimary :: a
    , colorTextSecondary :: a
    , displayBlock :: a
    , displayInline :: a
    , focusVisible :: a
    , gutterBottom :: a
    , h1 :: a
    , h2 :: a
    , h3 :: a
    , h4 :: a
    , h5 :: a
    , h6 :: a
    , noWrap :: a
    , overline :: a
    , paragraph :: a
    , root :: a
    , srOnly :: a
    , subtitle1 :: a
    , subtitle2 :: a
    , underlineAlways :: a
    , underlineHover :: a
    , underlineNone :: a
    )

type LinkClassesKey
  = LinkClassesGenericRow String

type LinkClassesJSS
  = LinkClassesGenericRow JSS

type LinkOptPropsRow (r :: # Type)
  = ( "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , children :: Array JSX
    , classes :: { | LinkClassesKey }
    , color :: Color
    , underline :: Underline
    , variant :: Variant
    | r
    )

type LinkReqPropsRow (r :: # Type)
  = r

type LinkPropsRow (r :: # Type)
  = LinkOptPropsRow (LinkReqPropsRow r)

foreign import _UnsafeLink :: forall componentProps. ReactComponent { | LinkPropsRow componentProps }

_Link ::
  forall given optionalGiven optionalMissing props required.
  Nub' (LinkReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (LinkPropsRow React.Basic.DOM.Props_a) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Link = unsafeCoerce _UnsafeLink

link ::
  forall given optionalGiven optionalMissing props required.
  Nub' (LinkReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (LinkPropsRow React.Basic.DOM.Props_a) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
link ps = element _Link ps

link' :: LinkProps -> JSX
link' = MUI.React.Basic.element _Link'

_Link' :: ReactComponent LinkProps
_Link' = unsafeCoerce _UnsafeLink

linkWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ LinkClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (LinkProps -> JSX)
linkWithStyles style = render
  where
  withStyles' :: ReactComponent LinkProps -> Effect.Effect (ReactComponent LinkProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Link'

  render = map MUI.React.Basic.element styledComponent

foreign import data LinkProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (LinkReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (LinkPropsRow React.Basic.DOM.Props_a) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> LinkProps
props = unsafeCoerce