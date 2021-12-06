{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HaskellPage.Source.Reddit
  ( getData,
    module T,
  )
where

import qualified Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import HaskellPage.Source.Reddit.Orphans ()
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import Network.Reddit.Types as T (Listing (..))
import qualified Network.Reddit.Types.Comment as T
import Network.Reddit.Types.Submission as T (Submission (..), SubmissionContent (..), SubmissionID (..))
import System.Directory (doesFileExist)

type SubmissionListing = T.Listing T.SubmissionID T.Submission

getData :: IO ([T.Submission], T.WithChildren)
getData = do
  Listing _ _ (toList -> posts) <-
    reqGetCached @SubmissionListing "reddit.haskell.front" $
      R.https "old.reddit.com" /: "r" /: "haskell" /: ".json"
  let (SubmissionID monthlyHaskId) = fromMaybe (error "No monthly haskell post") $ do
        Submission {..} <- head <$> nonEmpty posts
        guard $ "Monthly Hask Anything" `T.isPrefixOf` title
        pure submissionID
  -- FIXME: heddit sucks, because it doesn't expose contents of this lame
  -- 'WithChildren' type. Fork this library, or write my own `reddit-types`?
  monthlyHask <-
    reqGetCached @T.WithChildren ("reddit.haskell." <> toString monthlyHaskId) $
      R.https "old.reddit.com" /: "r" /: "haskell" /: "comments" /: monthlyHaskId /: ".json"
  pure (posts, monthlyHask)

reqGetCached ::
  forall a scheme.
  (Serialise.Serialise a, Aeson.FromJSON a) =>
  String ->
  R.Url scheme ->
  IO a
reqGetCached cacheId url = do
  cached (cacheId <> ".cbor") $ do
    print url
    res :: Aeson.Result a <- R.runReq R.defaultHttpConfig $ do
      response <- R.req R.GET url R.NoReqBody R.jsonResponse mempty
      print $ R.responseBody response
      pure $ Aeson.fromJSON $ R.responseBody response
    case res of
      Aeson.Error s ->
        error $ ("Error fetching " <> show url <> " because: ") <> toText s
      Aeson.Success list -> do
        pure list

-- Caching to cborg

cached :: Serialise.Serialise b => FilePath -> IO b -> IO b
cached fp f = do
  exists <- doesFileExist fp
  if exists
    then do
      putStrLn "Loading from cache"
      Serialise.readFileDeserialise fp
    else do
      putStrLn "Fetching data ..."
      res <- f
      putStrLn "Writing to cache"
      Serialise.writeFileSerialise fp res
      return res
