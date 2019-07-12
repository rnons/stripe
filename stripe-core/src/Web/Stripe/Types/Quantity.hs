{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Quantity where

import           Data.Aeson                     (FromJSON (parseJSON),
                                                 defaultOptions,
                                                 genericParseJSON,
                                                 unwrapUnaryRecords)
import           Data.Data                      (Data, Typeable)
import           GHC.Generics                   (Generic)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | Generic `Quantity` type to be used with `Customer`,
-- `Subscription` and `InvoiceLineItem` API requests
newtype Quantity = Quantity Int
    deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance FromJSON Quantity where
    parseJSON = genericParseJSON defaultOptions
        { unwrapUnaryRecords = True }

instance ToStripeParam Quantity where
  toStripeParam (Quantity i) =
    (("quantity", toBytestring i) :)
