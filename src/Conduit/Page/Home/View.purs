module Conduit.Page.Home.View where

import Prelude

import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Page.Home.Types (Action(..), State, StateTransition, Tab(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Effect (Effect)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

render ::
  forall p.
  State ->
  { send :: Action -> Effect Unit
  , state :: State
  , props :: p
  } ->
  React.JSX
render initialState { state, send } =
  container (guard (isNothing state.auth) banner)
    [ mainView initialState { state, send }
    , R.div
        { className: "col-md-3 col-xs-12"
        , children:
            [ R.div
                { className: "sidebar"
                , children:
                    [ R.p_ [ R.text "Popular Tags" ]
                    , renderTags { state, send }
                    ]
                }
            ]
        }
    ]

mainView :: State -> StateTransition -> React.JSX
mainView initialState { state, send } =
  R.div
    { className: "col-md-9 col-xs-12"
    , children:
        [ Tabs.tabs
            { className: "feed-toggle"
            , selectedTab: Just state.tab
            , tabs:
                [ { id: Feed
                  , label: R.text "Your Feed"
                  , disabled: isNothing state.auth
                  , content: tabContent { state, send }
                  }
                , { id: Global
                  , label: R.text "Global Feed"
                  , disabled: false
                  , content: tabContent { state, send }
                  }
                ]
                  <> case state.tab of
                      Tag tag ->
                        [ { id: Tag tag
                          , label:
                              React.fragment
                                [ R.i
                                    { className: "ion-pound"
                                    , children: []
                                    }
                                , R.text $ " " <> tag
                                ]
                          , disabled: false
                          , content: tabContent { state, send }
                          }
                        ]
                      _ -> []
            , onChange: \tab -> send $ LoadArticles tab initialState.pagination
            }
        ]
    }

tabContent :: StateTransition -> React.JSX
tabContent { state, send } =
  R.div_
    [ articleList
        { articles: state.articles <#> _.articles
        , onNavigate: send <<< Navigate
        , onFavoriteToggle: send <<< ToggleFavorite
        }
    , state.articles
        # RemoteData.maybe React.empty \{ articlesCount } ->
            pagination
              { offset: state.pagination.offset
              , limit: state.pagination.limit
              , totalCount: articlesCount
              , onChange: send <<< (LoadArticles state.tab)
              , focusWindow: 3
              , marginPages: 1
              }
    ]

renderTags :: StateTransition -> React.JSX
renderTags { state, send } = case state.tags of
  NotAsked -> R.div_ [ R.text "Tags not loaded" ]
  Loading -> R.div_ [ R.text "Loading Tags" ]
  Failure err -> R.div_ [ R.text $ "Failed loading tags" ]
  Success loadedTags -> R.div { className: "tag-list", children: map (\tag -> renderTag tag { state, send }) loadedTags }

renderTag :: String -> StateTransition -> React.JSX
renderTag tag { state, send } =
  R.a
    { className: "tag-default tag-pill"
    , href: "#"
    , onClick: handler preventDefault $ const $ send $ LoadArticles (Tag tag) state.pagination
    , children: [ R.text tag ]
    }

banner :: React.JSX
banner =
  R.div
    { className: "banner"
    , children:
        [ R.div
            { className: "container"
            , children:
                [ R.h1
                    { className: "logo-font"
                    , children: [ R.text "conduit" ]
                    }
                , R.p_
                    [ R.text "A place to share your knowledge." ]
                ]
            }
        ]
    }

container :: React.JSX -> Array React.JSX -> React.JSX
container header children =
  R.div
    { className: "home-page"
    , children:
        [ header
        , R.div
            { className: "container page"
            , children:
                [ R.div
                    { className: "row"
                    , children
                    }
                ]
            }
        ]
    }
