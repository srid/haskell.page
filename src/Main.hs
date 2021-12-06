{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import qualified Data.Ratio as Ratio
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import qualified HaskellPage.Source.Reddit as Reddit
import qualified Network.Reddit.Types as Reddit
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Printf (printf)

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

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
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
            showerDiv sub $ do
              H.div ! A.class_ "mt-4 flex flex-row justify-between" $ do
                H.div ! A.class_ "text-gray-400 w-16" $
                  redditVotesWidget sub
                H.div ! A.class_ "flex-1 mx-2" $
                  extLink ("https://old.reddit.com" <> permalink) $ H.toHtml title
                authorName $ Reddit.usernameToDisplayName author
              case content of
                Reddit.SelfText markdown _html ->
                  H.div $ do
                    -- TODO: render markdown
                    H.div ! A.class_ "h-16 overflow-hidden" $ H.toHtml markdown
                _ ->
                  pure ()

redditVotesWidget :: Reddit.Submission -> H.Html
redditVotesWidget sub = do
  let when1OrHigh mn (f :: Num a => a -> H.Html) = case mn of
        Just n | n > 0 -> f n
        _ -> pure ()
  when1OrHigh (Reddit.ups sub) $ \n ->
    "⬆" <> H.toHtml (show @Text n)
  let realDowns :: Maybe Double = do
        ups <- toRational <$> Reddit.ups sub
        ratio <- Reddit.upvoteRatio sub
        let inverseR :: Integral a => Ratio a -> Ratio a
            inverseR r = denominator r Ratio.% numerator r
        let rRation = inverseR ratio
            total = ups * rRation
            downs = total - ups
        pure $ fromRational downs
  when1OrHigh realDowns $ \n -> do
    "🔻"
    H.toHtml $ toText @String $ printf "%d" (round @Double @Int n)

extLink :: H.ToValue a => a -> H.Html -> H.Html
extLink url =
  H.a
    -- TODO: style visited links differently
    ! A.class_ "font-bold underline hover:bg-pink-100 hover:p-2"
    ! A.href (H.toValue url)

authorName :: H.ToMarkup a => a -> H.Html
authorName name =
  H.span ! A.class_ "text-gray-500" $ do
    "🧢"
    H.toHtml name

showerDiv :: Show a => a -> H.Html -> H.Html
showerDiv x w =
  H.div ! A.class_ "flex flex-col space-y-1 mt-4" $ do
    H.div ! A.class_ "flex-1" $ w
    let debugInfo = Shower.shower x
    H.div ! A.class_ "flex-1 max-h-32 overflow-auto text-gray-300 hover:text-gray-800" ! A.title (H.toValue debugInfo) $ do
      H.pre ! A.class_ "overflow-scroll text-xs" $ H.toHtml debugInfo