{- This module was autogenerated. Please don't edit. -}
module MUI.Core.TextField.FilledTextField where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.FormControl (FormControlPropsRow, FormControlReqPropsRow) as MUI.Core.FormControl
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Color :: Type

color ::
  { primary :: Color
  , secondary :: Color
  }
color = { primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary" }

foreign import data Margin :: Type

margin ::
  { dense :: Margin
  , none :: Margin
  , normal :: Margin
  }
margin = { dense: unsafeCoerce "dense", none: unsafeCoerce "none", normal: unsafeCoerce "normal" }

foreign import data Rows :: Type

rows ::
  { number :: Number -> Rows
  , string :: String -> Rows
  }
rows = { number: unsafeCoerce, string: unsafeCoerce }

foreign import data RowsMax :: Type

rowsMax ::
  { number :: Number -> RowsMax
  , string :: String -> RowsMax
  }
rowsMax = { number: unsafeCoerce, string: unsafeCoerce }

foreign import data Variant :: Type

variant ::
  { filled :: Variant
  }
variant = { filled: unsafeCoerce "filled" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqMargin :: Eq Margin where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

type FilledTextFieldClassesGenericRow a
  = ( root :: a
    )

type FilledTextFieldClassesKey
  = FilledTextFieldClassesGenericRow String

type FilledTextFieldClassesJSS
  = FilledTextFieldClassesGenericRow JSS

type FilledTextFieldOptPropsRow (r :: # Type)
  = ( autoComplete :: String
    , autoFocus :: Boolean
    , children :: Array JSX
    , classes :: { | FilledTextFieldClassesKey }
    , color :: Color
    , defaultValue :: Foreign.Foreign
    , disabled :: Boolean
    , error :: Boolean
    , fullWidth :: Boolean
    , helperText :: JSX
    , id :: String
    , label :: JSX
    , margin :: Margin
    , multiline :: Boolean
    , name :: String
    , onBlur :: React.Basic.Events.EventHandler
    , onChange :: React.Basic.Events.EventHandler
    , onFocus :: React.Basic.Events.EventHandler
    , placeholder :: String
    , required :: Boolean
    , rows :: Rows
    , rowsMax :: RowsMax
    , select :: Boolean
    , "type" :: String
    , value :: String
    | r
    )

type FilledTextFieldReqPropsRow (r :: # Type)
  = ( variant :: Variant
    | r
    )

type FilledTextFieldPropsRow (r :: # Type)
  = FilledTextFieldOptPropsRow (FilledTextFieldReqPropsRow r)

foreign import _UnsafeFilledTextField :: forall componentProps. ReactComponent { | FilledTextFieldPropsRow componentProps }

_FilledTextField ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FilledTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FilledTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_FilledTextField = unsafeCoerce _UnsafeFilledTextField

filledTextField ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FilledTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FilledTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
filledTextField ps = element _FilledTextField ps

filledTextField' :: FilledTextFieldProps -> JSX
filledTextField' = MUI.React.Basic.element _FilledTextField'

_FilledTextField' :: ReactComponent FilledTextFieldProps
_FilledTextField' = unsafeCoerce _UnsafeFilledTextField

filledTextFieldWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ FilledTextFieldClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (FilledTextFieldProps -> JSX)
filledTextFieldWithStyles style = render
  where
  withStyles' :: ReactComponent FilledTextFieldProps -> Effect.Effect (ReactComponent FilledTextFieldProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _FilledTextField'

  render = map MUI.React.Basic.element styledComponent

foreign import data FilledTextFieldProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FilledTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FilledTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> FilledTextFieldProps
props = unsafeCoerce