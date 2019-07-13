{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.CollectionMethod where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | Collection method of a `Subscription`
data CollectionMethod
    = ChargeAutomatically
    | SendInvoice
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CollectionMethod`
instance FromJSON CollectionMethod where
   parseJSON (String "charge_automatically") = pure ChargeAutomatically
   parseJSON (String "send_invoice")         = pure SendInvoice
   parseJSON _                               = mzero

instance ToStripeParam CollectionMethod where
  toStripeParam method =
    (( "collection_method"
     , case method of
          ChargeAutomatically -> "charge_automatically"
          SendInvoice         -> "send_invoice"
     ) :)
