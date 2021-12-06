module HaskellPage.SiteTypes where

import Ema
import qualified HaskellPage.Source.Reddit as Reddit

data Route
  = Index
  deriving (Show, Enum, Bounded)

data Model = Model
  { modelReddit :: [Reddit.Submission]
  }
  deriving (Eq, Show)

instance Ema Model Route where
  encodeRoute _model =
    \case
      Index -> "index.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    _ -> Nothing
  allRoutes _ =
    [ Index
    ]
