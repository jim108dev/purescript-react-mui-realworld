module Conduit.Page.Home.Main where

import Prelude

import Conduit.Component.App as App
import Conduit.Page.Home.Actions (handleAction)
import Conduit.Page.Home.Types (Action(..), Tab(..))
import Conduit.Page.Home.View (render)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..))
import React.Halo as Halo

mkHomePage :: App.Component Unit
mkHomePage = App.component "HomePage" { initialState, eval, render: render initialState }
  where
  initialState =
    { auth: Nothing
    , tags: NotAsked
    , articles: NotAsked
    , pagination: { offset: 0, limit: 10 }
    , tab: Global
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }
