{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Subscription where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Time                      (UTCTime)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Types.TaxRate       (TaxRateId (..))
import           Web.Stripe.Util                (mapWithIndex, toBytestring,
                                                 toSeconds)

------------------------------------------------------------------------------
-- | `SubscriptionId` for a `Subscription`
newtype SubscriptionId = SubscriptionId { getSubscriptionId :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionId`
instance FromJSON SubscriptionId where
    parseJSON (String x) = pure (SubscriptionId x)
    parseJSON _          = mzero

instance ToStripeParam SubscriptionId where
  toStripeParam (SubscriptionId sid) =
    (("subscription", Text.encodeUtf8 sid) :)

------------------------------------------------------------------------------
-- | `BillingCycleAnchor` for a `Subscription`
newtype BillingCycleAnchor = BillingCycleAnchor UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam BillingCycleAnchor where
  toStripeParam (BillingCycleAnchor time) =
    (("billing_cycle_anchor", toBytestring $ toSeconds time) :)

------------------------------------------------------------------------------
-- | Prorate
newtype Prorate = Prorate Bool deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Prorate where
  toStripeParam (Prorate p) =
    (("prorate", if p then "true" else "false") :)

------------------------------------------------------------------------------
-- | A flag that if set to true will delay the cancellation of the
-- subscription until the end of the current period.
newtype AtPeriodEnd = AtPeriodEnd Bool deriving (Read, Show, Eq, Ord, Data, Typeable)
instance ToStripeParam AtPeriodEnd where
  toStripeParam (AtPeriodEnd p) =
    (("at_period_end", if p then "true" else "false") :)

------------------------------------------------------------------------------
-- | `DefaultTaxRates` for a `Subscription`
newtype DefaultTaxRates = DefaultTaxRates [TaxRateId]
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam DefaultTaxRates where
  toStripeParam (DefaultTaxRates rates) xs =
    xs <> mapWithIndex (\n rate ->
      ( "default_tax_rates[" <> toBytestring n <> "]"
      , Text.encodeUtf8 $ (\(TaxRateId x) -> x) rate)
      ) rates

------------------------------------------------------------------------------
-- | Collection method of a `Subscription`
data SubscriptionCollectionMethod
    = ChargeAutomatically
    | SendInvoice
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionCollectionMethod`
instance FromJSON SubscriptionCollectionMethod where
   parseJSON (String "charge_automatically") = pure ChargeAutomatically
   parseJSON (String "send_invoice")         = pure SendInvoice
   parseJSON _                               = mzero

instance ToStripeParam SubscriptionCollectionMethod where
  toStripeParam method =
    (( "collection_method"
     , case method of
          ChargeAutomatically -> "charge_automatically"
          SendInvoice         -> "send_invoice"
     ) :)

------------------------------------------------------------------------------
-- | Status of a `Subscription`
data SubscriptionStatus
    = Incomplete
    | IncompleteExpired
    | Trialing
    | Active
    | PastDue
    | Canceled
    | UnPaid
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionStatus`
instance FromJSON SubscriptionStatus where
   parseJSON (String "incomplete")         = pure Incomplete
   parseJSON (String "incomplete_expired") = pure IncompleteExpired
   parseJSON (String "trialing")           = pure Trialing
   parseJSON (String "active")             = pure Active
   parseJSON (String "past_due")           = pure PastDue
   parseJSON (String "canceled")           = pure Canceled
   parseJSON (String "unpaid")             = pure UnPaid
   parseJSON _                             = mzero
