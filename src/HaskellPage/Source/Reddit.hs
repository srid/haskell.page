{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module HaskellPage.Source.Reddit where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import HaskellPage.Source.Reddit.Orphans ()
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Network.Reddit.Types as T
import qualified Network.Reddit.Types.Submission as T
import System.Directory (doesFileExist)

url :: R.Url 'R.Https
url = R.https "www.reddit.com" /: "r" /: "haskell" /: ".json"

type SubmissionListing = T.Listing T.SubmissionID T.Submission

cacheFile :: FilePath
cacheFile = "cborg.cache"

get :: IO SubmissionListing
get = do
  doesFileExist cacheFile >>= \case
    True -> do
      Serialise.readFileDeserialise cacheFile
    False -> do
      putStrLn "NOTE: Cache file not found, fetching from reddit"
      rListing :: Aeson.Result SubmissionListing <- R.runReq R.defaultHttpConfig $ do
        response <- R.req R.GET url R.NoReqBody R.jsonResponse mempty
        pure $ Aeson.fromJSON $ R.responseBody response
      case rListing of
        Aeson.Error s ->
          error $ toText s
        Aeson.Success list -> do
          Serialise.writeFileSerialise cacheFile list
          pure list