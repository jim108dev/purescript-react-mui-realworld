module Conduit.Page.Home.Action where

import Prelude
import Conduit.Capability.Auth (class MonadAuth, readAuth, readAuthEvent)
import Conduit.Capability.Resource.Article (class ArticleRepository, listArticles, listFeed, toggleFavorite)
import Conduit.Capability.Resource.Tag (class TagRepository, listTags)
import Conduit.Capability.Routing (class MonadRouting, navigate)
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Page.Home.Types (Action(..), State, Tab(..))
import Conduit.Page.Utils (_articles)
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..))
import Network.RemoteData as RemoteData
import React.Halo (HaloM)
import React.Halo as Halo

handleAction ::
  forall m a.
  MonadAuth m =>
  MonadRouting m =>
  TagRepository m =>
  ArticleRepository m =>
  Action -> HaloM a State Action m Unit
handleAction = case _ of
  Initialize -> do
    auth <- readAuth
    handleAction $ UpdateAuth auth
    authEvent <- readAuthEvent
    void $ Halo.subscribe $ map UpdateAuth authEvent
    handleAction LoadTags
  UpdateAuth auth -> do
    state <- Halo.get
    Halo.modify_ _ { auth = auth }
    case auth of
      Nothing -> handleAction $ LoadArticles state.tab state.pagination
      Just _ -> handleAction $ LoadArticles Feed state.pagination
  Navigate route -> do
    navigate route
  LoadTags -> do
    Halo.modify_ _ { tags = RemoteData.Loading }
    response <- listTags
    Halo.modify_ _ { tags = RemoteData.fromEither response }
  LoadArticles tab pagination -> do
    Halo.modify_ _ { articles = RemoteData.Loading, tab = tab, pagination = pagination }
    let
      query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
    response <- case tab of
      Feed -> listFeed query
      Global -> listArticles query
      Tag tag -> listArticles (query { tag = Just tag })
    Halo.modify_ _ { articles = RemoteData.fromEither response }
  ToggleFavorite ix -> do
    state <- Halo.get
    for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (Halo.modify_ <<< set (_articles ix)))
