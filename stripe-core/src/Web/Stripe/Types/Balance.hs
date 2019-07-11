{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Balance where

import           Data.Data                      (Data, Typeable)
import           Data.Time                      (UTCTime)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring, toSeconds)

------------------------------------------------------------------------------
-- | `AvailableOn`
newtype AvailableOn = AvailableOn UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AvailableOn where
  toStripeParam (AvailableOn time) =
    (("available_on", toBytestring $ toSeconds time) :)
