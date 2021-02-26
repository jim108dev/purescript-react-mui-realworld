{- This module was autogenerated. Please don't edit. -}
module MUI.Core.ButtonGroup where

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

foreign import data Color :: Type

color ::
  { default :: Color
  , inherit :: Color
  , primary :: Color
  , secondary :: Color
  }
color = { default: unsafeCoerce "default", inherit: unsafeCoerce "inherit", primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary" }

foreign import data Orientation :: Type

orientation ::
  { horizontal :: Orientation
  , vertical :: Orientation
  }
orientation = { horizontal: unsafeCoerce "horizontal", vertical: unsafeCoerce "vertical" }

foreign import data Size :: Type

size ::
  { large :: Size
  , medium :: Size
  , small :: Size
  }
size = { large: unsafeCoerce "large", medium: unsafeCoerce "medium", small: unsafeCoerce "small" }

foreign import data Variant :: Type

variant ::
  { contained :: Variant
  , outlined :: Variant
  , text :: Variant
  }
variant = { contained: unsafeCoerce "contained", outlined: unsafeCoerce "outlined", text: unsafeCoerce "text" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqSize :: Eq Size where
  eq = unsafeRefEq

instance eqOrientation :: Eq Orientation where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

instance eqAriaHaspopup :: Eq AriaHaspopup where
  eq = unsafeRefEq

type ButtonGroupClassesGenericRow a
  = ( contained :: a
    , disableElevation :: a
    , disabled :: a
    , fullWidth :: a
    , grouped :: a
    , groupedContained :: a
    , groupedContainedHorizontal :: a
    , groupedContainedPrimary :: a
    , groupedContainedSecondary :: a
    , groupedContainedVertical :: a
    , groupedHorizontal :: a
    , groupedOutlined :: a
    , groupedOutlinedHorizontal :: a
    , groupedOutlinedPrimary :: a
    , groupedOutlinedSecondary :: a
    , groupedOutlinedVertical :: a
    , groupedText :: a
    , groupedTextHorizontal :: a
    , groupedTextPrimary :: a
    , groupedTextSecondary :: a
    , groupedTextVertical :: a
    , groupedVertical :: a
    , root :: a
    , vertical :: a
    )

type ButtonGroupClassesKey
  = ButtonGroupClassesGenericRow String

type ButtonGroupClassesJSS
  = ButtonGroupClassesGenericRow JSS

type ButtonGroupOptPropsRow (r :: # Type)
  = ( "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , children :: Array JSX
    , classes :: { | ButtonGroupClassesKey }
    , color :: Color
    , disableFocusRipple :: Boolean
    , disableRipple :: Boolean
    , disabled :: Boolean
    , fullWidth :: Boolean
    , orientation :: Orientation
    , size :: Size
    , variant :: Variant
    | r
    )

type ButtonGroupReqPropsRow (r :: # Type)
  = r

type ButtonGroupPropsRow (r :: # Type)
  = ButtonGroupOptPropsRow (ButtonGroupReqPropsRow r)

foreign import _UnsafeButtonGroup :: forall componentProps. ReactComponent { | ButtonGroupPropsRow componentProps }

_ButtonGroup ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonGroupReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonGroupPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_ButtonGroup = unsafeCoerce _UnsafeButtonGroup

buttonGroup ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonGroupReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonGroupPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
buttonGroup ps = element _ButtonGroup ps

buttonGroup' :: ButtonGroupProps -> JSX
buttonGroup' = MUI.React.Basic.element _ButtonGroup'

_ButtonGroup' :: ReactComponent ButtonGroupProps
_ButtonGroup' = unsafeCoerce _UnsafeButtonGroup

buttonGroupWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ ButtonGroupClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (ButtonGroupProps -> JSX)
buttonGroupWithStyles style = render
  where
  withStyles' :: ReactComponent ButtonGroupProps -> Effect.Effect (ReactComponent ButtonGroupProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _ButtonGroup'

  render = map MUI.React.Basic.element styledComponent

foreign import data ButtonGroupProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (ButtonGroupReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (ButtonGroupPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> ButtonGroupProps
props = unsafeCoerce