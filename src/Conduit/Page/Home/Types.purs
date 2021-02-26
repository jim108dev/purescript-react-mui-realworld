module Conduit.Page.Home.Types where

import Prelude
import Conduit.Data.Article (Article)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Error (Error)
import Conduit.Data.Route (Route)
import Data.Maybe (Maybe)
import Effect (Effect)
import Network.RemoteData (RemoteData)

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

data Action
  = Initialize
  | UpdateAuth (Maybe Auth)
  | Navigate Route
  | LoadTags
  | LoadArticles Tab { offset :: Int, limit :: Int }
  | ToggleFavorite Int

type Pagination
  = { offset :: Int
    , limit :: Int
    }

type ArticlesDto
  = { articles :: Array Article, articlesCount :: Int }

type State
  = { auth :: Maybe Auth
    , tags :: RemoteData Error (Array String)
    , articles :: RemoteData Error (ArticlesDto)
    , pagination :: Pagination
    , tab :: Tab
    }

type StateTransition
  = { state :: State
    , send :: Action -> Effect Unit
    }
