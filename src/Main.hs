{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar
import qualified Ema
import HaskellPage.SiteTypes (Model (Model))
import qualified HaskellPage.Source.Reddit as Reddit
import qualified HaskellPage.View as View

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . View.render act m) $ \_act model -> do
    Reddit.Listing _ _ (toList -> posts) <- liftIO Reddit.getData
    LVar.set model $ Model posts
    putStrLn "Retrieved reddit data; not monitoring for changes"
    liftIO $ threadDelay maxBound
