{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaskellPage.Source.Reddit where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Network.Reddit.Types as T
import qualified Network.Reddit.Types.Award as T
import qualified Network.Reddit.Types.Submission as T
import qualified Network.Reddit.Types.Subreddit as T
import System.Directory (doesFileExist)

url :: R.Url 'R.Https
url = R.https "www.reddit.com" /: "r" /: "haskell" /: ".json"

type SubmissionListing = T.Listing T.SubmissionID T.Submission

deriving instance Serialise.Serialise T.SubmissionID

deriving instance Serialise.Serialise T.Username

deriving instance Serialise.Serialise T.SubmissionContent

deriving instance Serialise.Serialise T.SubredditName

deriving instance Serialise.Serialise T.SubredditID

deriving instance Serialise.Serialise T.ItemReport

deriving instance Serialise.Serialise T.Distinction

deriving instance Serialise.Serialise T.PollOption

deriving instance Serialise.Serialise T.AwardID

deriving instance Serialise.Serialise T.AwardType

deriving instance Serialise.Serialise T.Awarding

deriving instance Serialise.Serialise T.PollData

deriving instance Serialise.Serialise T.Submission

deriving instance (Serialise.Serialise a, Serialise.Serialise b) => Serialise.Serialise (T.Listing a b)

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