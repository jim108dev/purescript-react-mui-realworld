{- This module was autogenerated. Please don't edit. -}
module MUI.Core.TableFooter where

import Effect (Effect) as Effect
import Foreign (Foreign) as Foreign
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

type TableFooterClassesGenericRow a
  = ( root :: a
    )

type TableFooterClassesKey
  = TableFooterClassesGenericRow String

type TableFooterClassesJSS
  = TableFooterClassesGenericRow JSS

type TableFooterOptPropsRow (r :: # Type)
  = ( "aria-controls" :: String
    , "aria-haspopup" :: AriaHaspopup
    , children :: Array JSX
    , classes :: { | TableFooterClassesKey }
    , ref :: Foreign.Foreign
    | r
    )

type TableFooterReqPropsRow (r :: # Type)
  = r

type TableFooterPropsRow (r :: # Type)
  = TableFooterOptPropsRow (TableFooterReqPropsRow r)

foreign import _UnsafeTableFooter :: forall componentProps. ReactComponent { | TableFooterPropsRow componentProps }

_TableFooter ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableFooterReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableFooterPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_TableFooter = unsafeCoerce _UnsafeTableFooter

tableFooter ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableFooterReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableFooterPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
tableFooter ps = element _TableFooter ps

tableFooter' :: TableFooterProps -> JSX
tableFooter' = MUI.React.Basic.element _TableFooter'

_TableFooter' :: ReactComponent TableFooterProps
_TableFooter' = unsafeCoerce _UnsafeTableFooter

tableFooterWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ TableFooterClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (TableFooterProps -> JSX)
tableFooterWithStyles style = render
  where
  withStyles' :: ReactComponent TableFooterProps -> Effect.Effect (ReactComponent TableFooterProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _TableFooter'

  render = map MUI.React.Basic.element styledComponent

foreign import data TableFooterProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (TableFooterReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (TableFooterPropsRow React.Basic.DOM.Props_div) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> TableFooterProps
props = unsafeCoerce