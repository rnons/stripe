{-# LANGUAGE DeriveDataTypeable #-}
module Web.Stripe.Types.StripeTime where

import           Data.Aeson            (FromJSON (parseJSON))
import           Data.Data             (Data, Typeable)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | A newtype wrapper to parse timestamp to UTCTime
newtype StripeTime = StripeTime { getStripeTime :: UTCTime }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON StripeTime where
    parseJSON x = (StripeTime . posixSecondsToUTCTime) <$> parseJSON x
