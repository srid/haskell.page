{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | TODO: Upstream these instances? They may not be open to adding new
-- dependencies, however.
module HaskellPage.Source.Reddit.Orphans where

import qualified Codec.Serialise as Serialise
import qualified Network.Reddit.Types as T
import qualified Network.Reddit.Types.Award as T
import qualified Network.Reddit.Types.Submission as T
import qualified Network.Reddit.Types.Subreddit as T

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