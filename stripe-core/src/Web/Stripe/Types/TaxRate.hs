{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.TaxRate where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (mapWithIndex, toBytestring)

------------------------------------------------------------------------------
-- | `TaxRateId` for a `TaxRate`
newtype TaxRateId = TaxRateId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TaxRateId where
    parseJSON (String x) = pure $ TaxRateId x
    parseJSON _          = mzero

------------------------------------------------------------------------------
-- | `TaxRates` for a `SubscriptionItem`, `InvoiceItem`
newtype TaxRates = TaxRates [TaxRateId]
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TaxRates where
    parseJSON xs = TaxRates <$> parseJSON xs

instance ToStripeParam TaxRates where
  toStripeParam (TaxRates rates) xs =
    xs <> mapWithIndex (\n rate ->
      ( "tax_rates[" <> toBytestring n <> "]"
      , Text.encodeUtf8 $ (\(TaxRateId x) -> x) rate)
      ) rates
