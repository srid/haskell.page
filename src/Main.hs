{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.CLI as CLI
import qualified Ema.Helper.Markdown as Md
import qualified Ema.Helper.Tailwind as Tailwind
import qualified HaskellPage.Source.Reddit as Reddit
import qualified Network.Reddit.Types as Reddit
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \act model -> do
    when (act == CLI.Run) $ do
      Reddit.Listing _ _ (toList -> posts) <- liftIO Reddit.getData
      LVar.set model $ Model posts
      putStrLn "Got stuff"
      liftIO $ threadDelay maxBound

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r =
  Tailwind.layout emaAction (H.title "Haskell.Page" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      case r of
        Index -> do
          forM_ (modelReddit model) $ \sub@Reddit.Submission {..} -> do
            H.div ! A.class_ "flex flex-col space-y-1 mt-4" $ do
              H.div ! A.class_ "flex-1" $ do
                H.div ! A.class_ "flex flex-row justify-between" $ do
                  H.a ! A.class_ "font-bold" ! A.href (H.toValue $ "https://old.reddit.com" <> permalink) $ H.toHtml title
                  H.pre $ H.toHtml $ Reddit.usernameToDisplayName author
              H.div ! A.class_ "flex-1" $ do
                H.pre ! A.class_ "overflow-scroll text-xs" $ H.toHtml (Shower.shower sub)
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')