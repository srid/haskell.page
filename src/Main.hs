{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.CLI as CLI
import qualified Ema.Helper.Tailwind as Tailwind
import qualified HaskellPage.Source.Reddit as Reddit
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  deriving (Show, Enum, Bounded)

data Model = Model
  { modelReddit :: Reddit.SubmissionListing
  }
  deriving (Eq, Show)

instance Ema Model Route where
  encodeRoute _model =
    \case
      Index -> "index.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    _ -> Nothing

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \act model -> do
    when (act == CLI.Run) $ do
      posts <- liftIO Reddit.get
      LVar.set model $ Model posts
      putStrLn "Got stuff"
      liftIO $ threadDelay maxBound

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r =
  Tailwind.layout emaAction (H.title "Basic site" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        case r of
          Index -> do
            H.pre ! A.class_ "overflow-scroll text-xs" $ H.toHtml (Shower.shower model)
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')