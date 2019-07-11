{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Quantity where

import           Data.Data                      (Data, Typeable)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | Generic `Quantity` type to be used with `Customer`,
-- `Subscription` and `InvoiceLineItem` API requests
newtype Quantity = Quantity Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Quantity where
  toStripeParam (Quantity i) =
    (("quantity", toBytestring i) :)
