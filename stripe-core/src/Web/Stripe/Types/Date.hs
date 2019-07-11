{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Date where

import           Data.Data                      (Data, Typeable)
import           Data.Time                      (UTCTime)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring, toSeconds)

------------------------------------------------------------------------------
-- | `Created`
newtype Created = Created UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Created where
  toStripeParam (Created time) =
    (("created", toBytestring $ toSeconds time) :)

------------------------------------------------------------------------------
-- | `Date`
newtype Date = Date UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Date where
  toStripeParam (Date time) =
    (("created", toBytestring $ toSeconds time) :)
