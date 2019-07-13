{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.TaxRate where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String), camelTo2,
                                                 defaultOptions,
                                                 fieldLabelModifier,
                                                 genericParseJSON)
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           GHC.Generics                   (Generic)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Types.Metadata      (Metadata)
import           Web.Stripe.Types.StripeTime    (StripeTime)
import           Web.Stripe.Util                (mapWithIndex, toBytestring)

------------------------------------------------------------------------------
-- | `TaxRate` object
data TaxRate = TaxRate
    { id           :: TaxRateId
    , object       :: Text
    , active       :: Bool
    , created      :: StripeTime
    , description  :: Maybe Text
    , displayName  :: Text
    , inclusive    :: Bool
    , jurisdiction :: Text
    , livemode     :: Bool
    , metadata     :: Metadata
    , percentage   :: Float
    } deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance FromJSON TaxRate where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '_' }

------------------------------------------------------------------------------
-- | `TaxRateId` for a `TaxRate`
newtype TaxRateId = TaxRateId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TaxRateId where
    parseJSON (String x) = pure $ TaxRateId x
    parseJSON _          = mzero

------------------------------------------------------------------------------
-- | `DefaultTaxRates` for a `Subscription`, `Invoice`
newtype DefaultTaxRates = DefaultTaxRates [TaxRateId]
    deriving (Read, Show, Eq, Ord)
instance ToStripeParam DefaultTaxRates where
  toStripeParam (DefaultTaxRates rates) xs =
    xs <> mapWithIndex (\n rate ->
      ( "default_tax_rates[" <> toBytestring n <> "]"
      , Text.encodeUtf8 $ (\(TaxRateId x) -> x) rate)
      ) rates

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
