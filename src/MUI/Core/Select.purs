{- This module was autogenerated. Please don't edit. -}
module MUI.Core.Select where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.Input (InputPropsRow, InputReqPropsRow) as MUI.Core.Input
import MUI.Core.InputBase (InputBasePropsRow, InputBaseReqPropsRow) as MUI.Core.InputBase
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
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

foreign import data Variant :: Type

variant ::
  { filled :: Variant
  , outlined :: Variant
  , standard :: Variant
  }
variant = { filled: unsafeCoerce "filled", outlined: unsafeCoerce "outlined", standard: unsafeCoerce "standard" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqAriaHaspopup :: Eq AriaHaspopup where
  eq = unsafeRefEq

type SelectClassesGenericRow a
  = ( disabled :: a
    , filled :: a
    , icon :: a
    , iconFilled :: a
    , iconOpen :: a
    , iconOutlined :: a
    , outlined :: a
    , root :: a
    , select :: a
    , selectMenu :: a
    )

type SelectClassesKey
  = SelectClassesGenericRow String

type SelectClassesJSS
  = SelectClassesGenericRow JSS

type SelectOptPropsRow (r :: # Type)
  = ( "IconComponent" :: JSX
    , "MenuProps" :: Foreign.Foreign
    , "SelectDisplayProps" :: Foreign.Foreign
    , "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , autoWidth :: Boolean
    , children :: Array JSX
    , classes :: { | SelectClassesKey }
    , defaultValue :: Foreign.Foreign
    , displayEmpty :: Boolean
    , id :: String
    , input :: JSX
    , inputProps :: Foreign.Foreign
    , label :: JSX
    , labelId :: String
    , labelWidth :: Number
    , multiple :: Boolean
    , native :: Boolean
    , onChange :: React.Basic.Events.EventHandler
    , onClose :: React.Basic.Events.EventHandler
    , onOpen :: React.Basic.Events.EventHandler
    , open :: Boolean
    , ref :: Foreign.Foreign
    , renderValue :: {}
    , value :: String
    , variant :: Variant
    | r
    )

type SelectReqPropsRow (r :: # Type)
  = r

type SelectPropsRow (r :: # Type)
  = SelectOptPropsRow (SelectReqPropsRow r)

foreign import _UnsafeSelect :: forall componentProps. ReactComponent { | SelectPropsRow componentProps }

_Select ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SelectReqPropsRow (MUI.Core.Input.InputReqPropsRow (MUI.Core.InputBase.InputBaseReqPropsRow ()))) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SelectPropsRow (MUI.Core.Input.InputPropsRow (MUI.Core.InputBase.InputBasePropsRow React.Basic.DOM.Props_div))) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_Select = unsafeCoerce _UnsafeSelect

select ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SelectReqPropsRow (MUI.Core.Input.InputReqPropsRow (MUI.Core.InputBase.InputBaseReqPropsRow ()))) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SelectPropsRow (MUI.Core.Input.InputPropsRow (MUI.Core.InputBase.InputBasePropsRow React.Basic.DOM.Props_div))) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
select ps = element _Select ps

select' :: SelectProps -> JSX
select' = MUI.React.Basic.element _Select'

_Select' :: ReactComponent SelectProps
_Select' = unsafeCoerce _UnsafeSelect

selectWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ SelectClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (SelectProps -> JSX)
selectWithStyles style = render
  where
  withStyles' :: ReactComponent SelectProps -> Effect.Effect (ReactComponent SelectProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _Select'

  render = map MUI.React.Basic.element styledComponent

foreign import data SelectProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (SelectReqPropsRow (MUI.Core.Input.InputReqPropsRow (MUI.Core.InputBase.InputBaseReqPropsRow ()))) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (SelectPropsRow (MUI.Core.Input.InputPropsRow (MUI.Core.InputBase.InputBasePropsRow React.Basic.DOM.Props_div))) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> SelectProps
props = unsafeCoerce
