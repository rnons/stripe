{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.Stripe.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.Stripe.Types
  ( module Web.Stripe.Types
  , module Web.Stripe.Types.BankAccount
  , module Web.Stripe.Types.Card
  , module Web.Stripe.Types.Charge
  , module Web.Stripe.Types.Country
  , module Web.Stripe.Types.Currency
  , module Web.Stripe.Types.Customer
  , module Web.Stripe.Types.Coupon
  , module Web.Stripe.Types.Date
  , module Web.Stripe.Types.Description
  , module Web.Stripe.Types.Email
  , module Web.Stripe.Types.MetaData
  , module Web.Stripe.Types.Plan
  , module Web.Stripe.Types.Quantity
  , module Web.Stripe.Types.Recipient
  , module Web.Stripe.Types.StatementDescriptor
  , module Web.Stripe.Types.StripeList
  , module Web.Stripe.Types.Subscription
  , module Web.Stripe.Types.Token
  ) where
------------------------------------------------------------------------------
import           Control.Applicative                  (pure, (<$>), (<*>),
                                                       (<|>))
import           Control.Monad                        (mzero)
import           Data.Aeson                           (FromJSON (parseJSON), Value (Bool, Object, String),
                                                       (.:), (.:?))
import           Data.Data                            (Data, Typeable)
import qualified Data.HashMap.Strict                  as H
import           Data.Text                            (Text)
import           Data.Time                            (UTCTime)
import           Web.Stripe.StripeRequest.Class       (ToStripeParam (..))
import           Web.Stripe.Types.BankAccount         (AccountNumber (..),
                                                       BankAccountId (..),
                                                       BankAccountStatus (..),
                                                       NewBankAccount (..),
                                                       RoutingNumber (..))
import           Web.Stripe.Types.Card                (AddressCity (..),
                                                       AddressCountry (..),
                                                       AddressLine1 (..),
                                                       AddressLine2 (..),
                                                       AddressState (..),
                                                       AddressZip (..),
                                                       Brand (..), CVC (..),
                                                       CardId (..),
                                                       CardNumber (..),
                                                       DefaultCard (..),
                                                       ExpMonth (..),
                                                       ExpYear (..),
                                                       IsVerified (..),
                                                       Name (..), NewCard (..),
                                                       mkNewCard)
import           Web.Stripe.Types.Charge              (Amount (..),
                                                       ApplicationFeeAmount (..),
                                                       ApplicationFeeId (..),
                                                       ApplicationFeePercent (..),
                                                       ApplicationId (..),
                                                       ChargeId (..))
import           Web.Stripe.Types.Country             (Country (..))
import           Web.Stripe.Types.Coupon              (CouponId (..),
                                                       Duration (..),
                                                       IntervalCount (..),
                                                       TrialPeriodDays (..))
import           Web.Stripe.Types.Currency            (Currency (..))
import           Web.Stripe.Types.Customer            (CustomerId (..))
import           Web.Stripe.Types.Date                (Created (..), Date (..))
import           Web.Stripe.Types.Description         (Description (..))
import           Web.Stripe.Types.Dispute             (Evidence (..))
import           Web.Stripe.Types.Email               (Email (..))
import           Web.Stripe.Types.Event               (EventId (..), EventType)
import           Web.Stripe.Types.Invoice             (InvoiceId (..),
                                                       InvoiceLineItemId (..),
                                                       InvoiceLineItemType)
import           Web.Stripe.Types.InvoiceItem         (InvoiceItemId (..))
import           Web.Stripe.Types.MetaData            (MetaData (..))
import           Web.Stripe.Types.Plan                (Interval (..),
                                                       PlanId (..))
import           Web.Stripe.Types.Quantity            (Quantity (..))
import           Web.Stripe.Types.Recipient           (RecipientId (..),
                                                       RecipientType (..))
import           Web.Stripe.Types.Refund              (RefundId (..))
import           Web.Stripe.Types.StatementDescriptor (StatementDescription (..))
import           Web.Stripe.Types.StripeList          (EndingBefore (..),
                                                       Limit (..),
                                                       StartingAfter (..),
                                                       StripeList (..))
import           Web.Stripe.Types.Subscription        (BillingCycleAnchor (..),
                                                       DefaultTaxRates,
                                                       SubscriptionCollectionMethod,
                                                       SubscriptionId (..),
                                                       SubscriptionStatus)
import           Web.Stripe.Types.Token               (TokenId (..),
                                                       TokenType (..))
import           Web.Stripe.Types.Transaction         (TransactionId (..),
                                                       TransactionType (..))
import           Web.Stripe.Types.Transfer            (TransferId (..),
                                                       TransferStatus (..),
                                                       TransferType (..))
import           Web.Stripe.Util                      (fromSeconds,
                                                       toExpandable)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | `Expandable` values
--    maps from an id to an object, e.g. `CardId` to `Card`
type family ExpandsTo id :: *

-- | a wrapper for fields which can either be an id or an expanded object
data Expandable id
  = Id id -- ^ an id such as `CardId`, `AccountId`, `CustomerId`, etc
  | Expanded (ExpandsTo id) -- ^ expanded object such as `Card`, `Account`, `Customer`, etc
    deriving (Typeable)

deriving instance (Data id, Data (ExpandsTo id)) => Data (Expandable id)
deriving instance (Show id, Show (ExpandsTo id)) => Show (Expandable id)
deriving instance (Read id, Read (ExpandsTo id)) => Read (Expandable id)
deriving instance (Eq   id, Eq   (ExpandsTo id)) => Eq   (Expandable id)
deriving instance (Ord  id, Ord  (ExpandsTo id)) => Ord  (Expandable id)

type instance ExpandsTo AccountId       = Account
type instance ExpandsTo CardId          = Card
type instance ExpandsTo ChargeId        = Charge
type instance ExpandsTo CustomerId      = Customer
type instance ExpandsTo InvoiceId       = Invoice
type instance ExpandsTo InvoiceItemId   = InvoiceItem
type instance ExpandsTo RecipientId     = Recipient
type instance ExpandsTo RecipientCardId = RecipientCard
type instance ExpandsTo TransactionId   = BalanceTransaction

------------------------------------------------------------------------------
-- | JSON Instance for `Expandable`
instance (FromJSON id,  FromJSON (ExpandsTo id)) =>
         FromJSON (Expandable id) where
  parseJSON v = (Id <$> parseJSON v) <|> (Expanded <$> parseJSON v)

------------------------------------------------------------------------------
-- | `Charge` object in `Stripe` API
data Charge = Charge {
      chargeId                   :: ChargeId
    , chargeObject               :: Text
    , chargeCreated              :: UTCTime
    , chargeLiveMode             :: Bool
    , chargePaid                 :: Bool
    , chargeAmount               :: Amount
    , chargeCurrency             :: Currency
    , chargeRefunded             :: Bool
    , chargeCreditCard           :: Maybe Card
    , chargeCaptured             :: Bool
    , chargeRefunds              :: StripeList Refund
    , chargeBalanceTransaction   :: Maybe (Expandable TransactionId)
    , chargeFailureMessage       :: Maybe Text
    , chargeFailureCode          :: Maybe Text
    , chargeAmountRefunded       :: Int
    , chargeCustomerId           :: Maybe (Expandable CustomerId)
    , chargeInvoice              :: Maybe (Expandable InvoiceId)
    , chargeDescription          :: Maybe Description
    , chargeDispute              :: Maybe Dispute
    , chargeMetaData             :: MetaData
    , chargeStatementDescription :: Maybe StatementDescription
    , chargeReceiptEmail         :: Maybe Text
    , chargeReceiptNumber        :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Charge`
instance FromJSON Charge where
    parseJSON (Object o) =
        Charge <$> (ChargeId <$> o .: "id")
               <*> o .: "object"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "livemode"
               <*> o .: "paid"
               <*> (Amount <$> o .: "amount")
               <*> o .: "currency"
               <*> o .: "refunded"
               <*> o .:? "card"
               <*> o .: "captured"
               <*> o .: "refunds"
               <*> o .:? "balance_transaction"
               <*> o .:? "failure_message"
               <*> o .:? "failure_code"
               <*> o .: "amount_refunded"
               <*> o .:? "customer"
               <*> o .:? "invoice"
               <*> o .:? "description"
               <*> o .:? "dispute"
               <*> o .: "metadata"
               <*> o .:? "statement_description"
               <*> o .:? "receipt_email"
               <*> o .:? "receipt_number"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Refund` Object
data Refund = Refund {
      refundId                 :: RefundId
    , refundAmount             :: Int
    , refundCurrency           :: Currency
    , refundCreated            :: UTCTime
    , refundObject             :: Text
    , refundCharge             :: ChargeId
    , refundBalanceTransaction :: Maybe (Expandable TransactionId)
    , refundMetaData           :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Refund`
instance FromJSON Refund where
   parseJSON (Object o) =
        Refund <$> (RefundId <$> o .: "id")
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "object"
               <*> o .: "charge"
               <*> o .:? "balance_transaction"
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Customer` object
data Customer = Customer {
      customerObject         :: Text
    , customerCreated        :: UTCTime
    , customerId             :: CustomerId
    , customerLiveMode       :: Bool
    , customerDescription    :: Maybe Description
    , customerEmail          :: Maybe Email
    , customerDelinquent     :: Bool
    , customerSubscriptions  :: StripeList Subscription
    , customerDiscount       :: Maybe Discount
    , customerAccountBalance :: Int
    -- , customerCards          :: StripeList Card
    , customerCurrency       :: Maybe Currency
    , customerDefaultCard    :: Maybe (Expandable CardId)
    , customerMetaData       :: MetaData
    } | DeletedCustomer {
      deletedCustomer   :: Maybe Bool
    , deletedCustomerId :: CustomerId
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Customer`
instance FromJSON Customer where
    parseJSON (Object o)
        = (Customer
           <$> o .: "object"
           <*> (fromSeconds <$> o .: "created")
           <*> (CustomerId <$> o .: "id")
           <*> o .: "livemode"
           <*> o .:? "description"
           <*> (fmap Email <$> o .:? "email")
           <*> o .: "delinquent"
           <*> o .: "subscriptions"
           <*> o .:? "discount"
           <*> o .: "account_balance"
           -- <*> o .: "cards"
           <*> o .:? "currency"
           <*> o .:? "default_card"
           <*> o .: "metadata"
          )
           -- <|> DeletedCustomer
           -- <$> o .: "deleted"
           -- <*> (CustomerId <$> o .: "id"))
           -- <|> DeletedCustomer
           -- <$> o .:? "deleted"
           -- <*> (CustomerId <$> o .: "id")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | CardId for a `Recipient`
newtype RecipientCardId = RecipientCardId Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientCardId`
instance FromJSON RecipientCardId where
   parseJSON (String x) = pure $ RecipientCardId x
   parseJSON _          = mzero

------------------------------------------------------------------------------
-- | `Card` Object
data Card = Card {
      cardId                :: CardId
    , cardObject            :: Text
    , cardLastFour          :: Text
    , cardBrand             :: Brand
    , cardFunding           :: Text
    , cardExpMonth          :: ExpMonth
    , cardExpYear           :: ExpYear
    , cardFingerprint       :: Text
    , cardCountry           :: Maybe Text
    , cardName              :: Maybe Name
    , cardAddressLine1      :: Maybe AddressLine1
    , cardAddressLine2      :: Maybe AddressLine2
    , cardAddressCity       :: Maybe AddressCity
    , cardAddressState      :: Maybe AddressState
    , cardAddressZip        :: Maybe AddressZip
    , cardAddressCountry    :: Maybe AddressCountry
    , cardCVCCheck          :: Maybe Text
    , cardAddressLine1Check :: Maybe Text
    , cardAddressZipCheck   :: Maybe Text
    , cardCustomerId        :: Maybe (Expandable CustomerId)
    , cardMetaData          :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RecipientCard` object
data RecipientCard = RecipientCard {
      recipientCardId                :: RecipientCardId
    , recipientCardLastFour          :: Text
    , recipientCardBrand             :: Brand
    , recipientCardFunding           :: Text
    , recipientCardExpMonth          :: ExpMonth
    , recipientCardExpYear           :: ExpYear
    , recipientCardFingerprint       :: Text
    , recipientCardCountry           :: Country
    , recipientCardName              :: Maybe Name
    , recipientCardAddressLine1      :: Maybe AddressLine1
    , recipientCardAddressLine2      :: Maybe AddressLine2
    , recipientCardAddressCity       :: Maybe AddressCity
    , recipientCardAddressState      :: Maybe AddressState
    , recipientCardAddressZip        :: Maybe AddressZip
    , recipientCardAddressCountry    :: Maybe AddressCountry
    , recipientCardCVCCheck          :: Maybe Text
    , recipientCardAddressLine1Check :: Maybe Text
    , recipientCardAddressZipCheck   :: Maybe Text
    , recipientCardRecipientId       :: Maybe (Expandable RecipientId)
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Card`
instance FromJSON Card where
    parseJSON (Object o) =
        Card <$> (CardId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> (ExpMonth <$> o .: "exp_month")
             <*> (ExpYear <$> o .: "exp_year")
             <*> o .: "fingerprint"
             <*> o .:? "country"
             <*> o .:? "name"
             <*> (fmap AddressLine1 <$> o .:? "address_line1")
             <*> (fmap AddressLine2 <$> o .:? "address_line2")
             <*> (fmap AddressCity <$> o .:? "address_city")
             <*> (fmap AddressState <$> o .:? "address_state")
             <*> (fmap AddressZip <$> o .:? "address_zip")
             <*> (fmap AddressCountry <$> o .:? "address_country")
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "customer"
             <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientCard`
instance FromJSON RecipientCard where
    parseJSON (Object o) =
       RecipientCard
             <$> (RecipientCardId <$> o .: "id")
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> (ExpMonth <$> o .: "exp_month")
             <*> (ExpYear <$> o .: "exp_year")
             <*> o .: "fingerprint"
             <*> (Country <$> o .: "country")
             <*> o .:? "name"
             <*> (fmap AddressLine1 <$> o .:? "address_line1")
             <*> (fmap AddressLine2 <$> o .:? "address_line2")
             <*> (fmap AddressCity <$> o .:? "address_city")
             <*> (fmap AddressState <$> o .:? "address_state")
             <*> (fmap AddressZip <$> o .:? "address_zip")
             <*> (fmap AddressCountry <$> o .:? "address_country")
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "recipient"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Subscription Object
data Subscription = Subscription {
      subscriptionId                    :: SubscriptionId
    , subscriptionObject                :: Text
    , subscriptionApplicationFeePercent :: Maybe Double
    -- , subscriptionBilling
    , subscriptionBillingCycleAnchor    :: BillingCycleAnchor
    -- , subscriptionBillingThresholds
    , subscriptionCancelAtPeriodEnd     :: Bool
    , subscriptionCanceledAt            :: Maybe UTCTime
    , subscriptionCollectionMethod      :: SubscriptionCollectionMethod
    , subscriptionCreated               :: UTCTime
    , subscriptionCurrentPeriodEnd      :: UTCTime
    , subscriptionCurrentPeriodStart    :: UTCTime
    , subscriptionCustomerId            :: Expandable CustomerId
    , subscriptionDaysUntilDue          :: Maybe Int
    -- , subscriptionDefaultPaymentMethod
    -- , subscriptionDefaultSource
    , subscriptionDefaultTaxRates       :: DefaultTaxRates
    , subscriptionDiscount              :: Maybe Discount
    , subscriptionEndedAt               :: Maybe UTCTime
    -- , subscriptionItems
    -- , subscriptionLatestInvoice
    , subscriptionLiveMode              :: Bool
    , subscriptionMetaData              :: MetaData
    , subscriptionPlan                  :: Maybe Plan
    , subscriptionQuantity              :: Quantity
    , subscriptionStart                 :: UTCTime
    , subscriptionStartDate             :: UTCTime
    , subscriptionStatus                :: SubscriptionStatus
    , subscriptionTrialEnd              :: Maybe UTCTime
    , subscriptionTrialStart            :: Maybe UTCTime
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Subscription`
instance FromJSON Subscription where
    parseJSON (Object o) = Subscription
        <$> (SubscriptionId <$> o .: "id")
        <*> o .: "object"
        <*> o .:? "application_fee_percent"
        <*> (BillingCycleAnchor . fromSeconds <$> o .: "billing_cycle_anchor")
        <*> o .: "cancel_at_period_end"
        <*> (fmap fromSeconds <$> o .:? "canceled_at")
        <*> o .: "collection_method"
        <*> (fromSeconds <$> o .: "created")
        <*> (fromSeconds <$> o .: "current_period_end")
        <*> (fromSeconds <$> o .: "current_period_start")
        <*> o .: "customer"
        <*> o .:? "days_until_due"
        <*> o .: "default_tax_rates"
        <*> o .:? "discount"
        <*> (fmap fromSeconds <$> o .:? "ended_at")
        <*> o .: "livemode"
        <*> o .: "metadata"
        <*> o .: "plan"
        <*> (Quantity <$> o .:  "quantity")
        <*> (fromSeconds <$> o .: "start")
        <*> (fromSeconds <$> o .: "start_date")
        <*> o .: "status"
        <*> (fmap fromSeconds <$> o .:? "trial_end")
        <*> (fmap fromSeconds <$> o .:? "trial_start")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Plan object
data Plan = Plan {
      planInterval        :: Interval
    -- , planName            :: Text
    , planCreated         :: UTCTime
    , planAmount          :: Int
    , planCurrency        :: Currency
    , planId              :: PlanId
    , planObject          :: Text
    , planLiveMode        :: Bool
    , planIntervalCount   :: Maybe Int -- optional, max of 1 year intervals allowed, default 1
    , planTrialPeriodDays :: Maybe Int
    , planMetaData        :: MetaData
    , planDescription     :: Maybe StatementDescription
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Plan`
instance FromJSON Plan where
   parseJSON (Object o) =
        Plan <$> o .: "interval"
             -- <*> o .: "name"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "amount"
             <*> o .: "currency"
             <*> (PlanId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "livemode"
             <*> o .:? "interval_count"
             <*> o .:? "trial_period_days"
             <*> o .: "metadata"
             <*> o .:? "statement_description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `TrialPeriod` for a Plan
newtype TrialPeriod = TrialPeriod UTCTime deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Coupon` Object
data Coupon = Coupon {
      couponId               :: CouponId
    , couponCreated          :: UTCTime
    , couponPercentOff       :: Maybe Float
    , couponAmountOff        :: Maybe Int
    , couponCurrency         :: Maybe Currency
    , couponLiveMode         :: Bool
    , couponDuration         :: Duration
    , couponRedeemBy         :: Maybe UTCTime
    , couponMaxRedemptions   :: Maybe Int
    , couponTimesRedeemed    :: Maybe Int
    , couponDurationInMonths :: Maybe Int
    , couponValid            :: Bool
    , couponMetaData         :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Coupon`
instance FromJSON Coupon where
   parseJSON (Object o) =
        Coupon <$> (CouponId <$> o .: "id")
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "percent_off"
               <*> o .:? "amount_off"
               <*> o .:? "currency"
               <*> o .: "livemode"
               <*> o .: "duration"
               <*> (fmap fromSeconds <$> o .:? "redeem_by")
               <*> o .:? "max_redemptions"
               <*> o .:? "times_redeemed"
               <*> o .:? "duration_in_months"
               <*> o .: "valid"
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Discount` for `Coupon`
data Discount = Discount {
      discountCoupon       :: Coupon
    , discountStart        :: UTCTime
    , discountEnd          :: Maybe UTCTime
    , discountCustomer     :: Expandable CustomerId
    , discountObject       :: Text
    , discountSubscription :: Maybe SubscriptionId
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Discount`
instance FromJSON Discount where
    parseJSON (Object o) =
        Discount <$> o .: "coupon"
                 <*> (fromSeconds <$> o .: "start")
                 <*> (fmap fromSeconds <$> o .:? "end")
                 <*> o .: "customer"
                 <*> o .: "object"
                 <*> (fmap SubscriptionId <$> o .:? "subscription")
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Invoice` Object
data Invoice = Invoice {
      invoiceDate                 :: UTCTime
    , invoiceId                   :: Maybe InvoiceId -- ^ If upcoming no ID will exist
    , invoicePeriodStart          :: UTCTime
    , invoicePeriodEnd            :: UTCTime
    , invoiceLineItems            :: StripeList InvoiceLineItem
    , invoiceSubTotal             :: Int
    , invoiceTotal                :: Int
    , invoiceCustomer             :: Expandable CustomerId
    , invoiceObject               :: Text
    , invoiceAttempted            :: Bool
    , invoiceClosed               :: Bool
    , invoiceForgiven             :: Bool
    , invoicePaid                 :: Bool
    , invoiceLiveMode             :: Bool
    , invoiceAttemptCount         :: Int
    , invoiceAmountDue            :: Int
    , invoiceCurrency             :: Currency
    , invoiceStartingBalance      :: Int
    , invoiceEndingBalance        :: Maybe Int
    , invoiceNextPaymentAttempt   :: Maybe UTCTime
    , invoiceWebHooksDeliveredAt  :: Maybe UTCTime
    , invoiceCharge               :: Maybe (Expandable ChargeId)
    , invoiceDiscount             :: Maybe Discount
    , invoiceApplicateFee         :: Maybe FeeId
    , invoiceSubscription         :: Maybe SubscriptionId
    , invoiceStatementDescription :: Maybe StatementDescription
    , invoiceDescription          :: Maybe Description
    , invoiceMetaData             :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Invoice`
instance FromJSON Invoice where
   parseJSON (Object o) =
       Invoice <$> (fromSeconds <$> o .: "date")
               <*> (fmap InvoiceId <$> o .:? "id")
               <*> (fromSeconds <$> o .: "period_start")
               <*> (fromSeconds <$> o .: "period_end")
               <*> o .: "lines"
               <*> o .: "subtotal"
               <*> o .: "total"
               <*> o .: "customer"
               <*> o .: "object"
               <*> o .: "attempted"
               <*> o .: "closed"
               <*> o .: "forgiven"
               <*> o .: "paid"
               <*> o .: "livemode"
               <*> o .: "attempt_count"
               <*> o .: "amount_due"
               <*> o .: "currency"
               <*> o .: "starting_balance"
               <*> o .:? "ending_balance"
               <*> (fmap fromSeconds <$> o .:? "next_payment_attempt")
               <*> (fmap fromSeconds <$> o .: "webhooks_delivered_at")
               <*> o .:? "charge"
               <*> o .:? "discount"
               <*> (fmap FeeId <$> o .:? "application_fee")
               <*> (fmap SubscriptionId <$> o .: "subscription")
               <*> o .:? "statement_description"
               <*> o .:? "description"
               <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceItem` object
data InvoiceItem = InvoiceItem {
      invoiceItemObject       :: Text
    , invoiceItemId           :: InvoiceItemId
    , invoiceItemDate         :: UTCTime
    , invoiceItemAmount       :: Int
    , invoiceItemLiveMode     :: Bool
    , invoiceItemProration    :: Bool
    , invoiceItemCurrency     :: Currency
    , invoiceItemCustomer     :: Expandable CustomerId
    , invoiceItemDescription  :: Maybe Description
    , invoiceItemInvoice      :: Maybe (Expandable InvoiceId)
    , invoiceItemQuantity     :: Maybe Quantity
    , invoiceItemSubscription :: Maybe SubscriptionId
    , invoiceItemMetaData     :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceItem`
instance FromJSON InvoiceItem where
   parseJSON (Object o) =
       InvoiceItem <$> o .: "object"
                   <*> (InvoiceItemId <$> o .: "id")
                   <*> (fromSeconds <$> o .: "date")
                   <*> o .: "amount"
                   <*> o .: "livemode"
                   <*> o .: "proration"
                   <*> o .: "currency"
                   <*> o .: "customer"
                   <*> o .:? "description"
                   <*> o .:? "invoice"
                   <*> (fmap Quantity <$> o .:? "quantity")
                   <*> o .:? "subscription"
                   <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceLineItem` Object
data InvoiceLineItem = InvoiceLineItem {
      invoiceLineItemId          :: InvoiceLineItemId
    , invoiceLineItemObject      :: Text
    , invoiceLineItemType        :: InvoiceLineItemType
    , invoiceLineItemLiveMode    :: Bool
    , invoiceLineItemAmount      :: Int
    , invoiceLineItemCurrency    :: Currency
    , invoiceLineItemProration   :: Bool
    , invoiceLineItemPeriod      :: Period
    , invoiceLineItemQuantity    :: Maybe Quantity
    , invoiceLineItemPlan        :: Maybe Plan
    , invoiceLineItemDescription :: Maybe Description
    , invoiceLineItemMetaData    :: MetaData
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Period for an `InvoiceLineItem`
data Period = Period {
      start :: UTCTime
    , end   :: UTCTime
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Period`
instance FromJSON Period where
   parseJSON (Object o) =
       Period <$> (fromSeconds <$> o .: "start")
              <*> (fromSeconds <$> o .: "end")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItem`
instance FromJSON InvoiceLineItem where
   parseJSON (Object o) =
       InvoiceLineItem <$> (InvoiceLineItemId <$> o .: "id")
                       <*> o .: "object"
                       <*> o .: "type"
                       <*> o .: "livemode"
                       <*> o .: "amount"
                       <*> o .: "currency"
                       <*> o .: "proration"
                       <*> o .: "period"
                       <*> (fmap Quantity <$> o .:? "quantity")
                       <*> o .:? "plan"
                       <*> o .:? "description"
                       <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Status of a `Dispute`
data DisputeStatus
    = WarningNeedsResponse
    | WarningUnderReview
    | NeedsResponse
    | UnderReview
    | ChargeRefunded
    | Won
    | Lost
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeReason`
instance FromJSON DisputeReason where
   parseJSON (String "duplicate")             = pure Duplicate
   parseJSON (String "fraudulent")            = pure Fraudulent
   parseJSON (String "subscription_canceled") = pure SubscriptionCanceled
   parseJSON (String "product_unacceptable")  = pure ProductUnacceptable
   parseJSON (String "product_not_received")  = pure ProductNotReceived
   parseJSON (String "credit_not_processed")  = pure CreditNotProcessed
   parseJSON (String "general")               = pure General
   parseJSON _                                = mzero

------------------------------------------------------------------------------
-- | Reason of a `Dispute`
data DisputeReason
    = Duplicate
    | Fraudulent
    | SubscriptionCanceled
    | ProductUnacceptable
    | ProductNotReceived
    | Unrecognized
    | CreditNotProcessed
    | General
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeStatus`
instance FromJSON DisputeStatus where
   parseJSON (String "needs_response")         = pure NeedsResponse
   parseJSON (String "warning_needs_response") = pure WarningNeedsResponse
   parseJSON (String "warning_under_review")   = pure WarningUnderReview
   parseJSON (String "under_review")           = pure UnderReview
   parseJSON (String "charge_refunded")        = pure ChargeRefunded
   parseJSON (String "won")                    = pure Won
   parseJSON (String "lost")                   = pure Lost
   parseJSON _                                 = mzero

------------------------------------------------------------------------------
-- | `Dispute` Object
data Dispute = Dispute {
      disputeChargeId            :: Expandable ChargeId
    , disputeAmount              :: Int
    , disputeCreated             :: UTCTime
    , disputeStatus              :: DisputeStatus
    , disputeLiveMode            :: Bool
    , disputeCurrency            :: Currency
    , disputeObject              :: Text
    , disputeReason              :: DisputeReason
    , disputeIsChargeRefundable  :: Bool
    , disputeBalanceTransactions :: [BalanceTransaction]
    , disputeEvidenceDueBy       :: UTCTime
    , disputeEvidence            :: Maybe Evidence
    , disputeMetaData            :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Dispute`
instance FromJSON Dispute where
    parseJSON (Object o) =
        Dispute <$> o .: "charge"
                <*> o .: "amount"
                <*> (fromSeconds <$> o .: "created")
                <*> o .: "status"
                <*> o .: "livemode"
                <*> o .: "currency"
                <*> o .: "object"
                <*> o .: "reason"
                <*> o .: "is_charge_refundable"
                <*> o .: "balance_transactions"
                <*> (fromSeconds <$> o .: "evidence_due_by")
                <*> (fmap Evidence <$> o .:? "evidence")
                <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Transfer` Object
data Transfer = Transfer {
      transferId                    :: TransferId
     , transferObject               :: Text
     , transferCreated              :: UTCTime
     , transferDate                 :: UTCTime
     , transferLiveMode             :: Bool
     , transferAmount               :: Int
     , transferCurrency             :: Currency
     , transferStatus               :: TransferStatus
     , transferType                 :: TransferType
     , transferBalanceTransaction   :: Expandable TransactionId
     , transferDescription          :: Maybe Description
     , transferBankAccount          :: Maybe BankAccount
     , transferFailureMessage       :: Maybe Text
     , transferFailureCode          :: Maybe Text
     , transferStatementDescription :: Maybe StatementDescription
     , transferRecipient            :: Maybe (Expandable RecipientId)
     , transferMetaData             :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Transfer`
instance FromJSON Transfer where
    parseJSON (Object o) =
        Transfer <$> (TransferId <$> o .: "id")
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "created")
                    <*> (fromSeconds <$> o .: "date")
                    <*> o .: "livemode"
                    <*> o .: "amount"
                    <*> o .: "currency"
                    <*> o .: "status"
                    <*> o .: "type"
                    <*> o .: "balance_transaction"
                    <*> o .:? "description"
                    <*> o .:? "bank_account"
                    <*> o .:? "failure_message"
                    <*> o .:? "failure_code"
                    <*> o .:? "statement_description"
                    <*> o .:? "recipient"
                    <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BankAccount` Object
data BankAccount = BankAccount {
      bankAccountId          :: BankAccountId
    , bankAccountObject      :: Text
    , bankAccountLast4       :: Text
    , bankAccountCountry     :: Country
    , bankAccountCurrency    :: Currency
    , bankAccountStatus      :: Maybe BankAccountStatus
    , bankAccountFingerprint :: Maybe Text
    , bankAccountName        :: Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccount` JSON Instance
instance FromJSON BankAccount where
   parseJSON (Object o) =
     BankAccount <$> (BankAccountId <$> o .: "id")
                 <*> o .: "object"
                 <*> o .: "last4"
                 <*> (Country <$> o .: "country")
                 <*> o .: "currency"
                 <*> o .:? "status"
                 <*> o .:? "fingerprint"
                 <*> o .: "bank_name"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Recipients

------------------------------------------------------------------------------
-- | `FirstName` of a `Recipient`
newtype FirstName = FirstName Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `LastName` of a `Recipient`
newtype LastName = LastName Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Middle Initial of a `Recipient`
type MiddleInitial = Char

------------------------------------------------------------------------------
-- | Recipient Object
data Recipient = Recipient {
      recipientId            :: RecipientId
    , recipientObject        :: Text
    , recipientCreated       :: UTCTime
    , recipientLiveMode      :: Bool
    , recipientType          :: RecipientType
    , recipientDescription   :: Maybe Description
    , recipientEmail         :: Maybe Email
    , recipientName          :: Name
    , recipientVerified      :: Bool
    , recipientActiveAccount :: Maybe BankAccount
    , recipientCards         :: StripeList RecipientCard
    , recipientDefaultCard   :: Maybe (Expandable RecipientCardId)
 } | DeletedRecipient {
    deletedRecipient   :: Maybe Bool
  , deletedRecipientId :: RecipientId
 } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Recipient`
instance FromJSON Recipient where
   parseJSON (Object o) =
      (Recipient <$> (RecipientId <$> o .: "id")
                 <*> o .: "object"
                 <*> (fromSeconds <$> o .: "created")
                 <*> o .: "livemode"
                 <*> o .: "type"
                 <*> o .:? "description"
                 <*> (fmap Email <$> o .:? "email")
                 <*> o .: "name"
                 <*> o .: "verified"
                 <*> o .:? "active_account"
                 <*> o .: "cards"
                 <*> o .:? "default_card"
      )
      <|> DeletedRecipient
                 <$> o .:? "deleted"
                 <*> (RecipientId <$> o .: "id")

   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | ApplicationFee Object
data ApplicationFee = ApplicationFee {
      applicationFeeId                 :: ApplicationFeeId
    , applicationFeeObjecet            :: Text
    , applicationFeeCreated            :: UTCTime
    , applicationFeeLiveMode           :: Bool
    , applicationFeeAmount             :: Int
    , applicationFeeCurrency           :: Currency
    , applicationFeeRefunded           :: Bool
    , applicationFeeAmountRefunded     :: Int
    , applicationFeeRefunds            :: StripeList Refund
    , applicationFeeBalanceTransaction :: Expandable TransactionId
    , applicationFeeAccountId          :: Expandable AccountId
    , applicationFeeApplicationId      :: ApplicationId
    , applicationFeeChargeId           :: Expandable ChargeId
    , applicationFeeMetaData           :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFee`
instance FromJSON ApplicationFee where
   parseJSON (Object o) =
       ApplicationFee <$> (ApplicationFeeId <$> o .: "id")
                      <*> o .: "object"
                      <*> (fromSeconds <$> o .: "created")
                      <*> o .: "livemode"
                      <*> o .: "amount"
                      <*> o .: "currency"
                      <*> o .: "refunded"
                      <*> o .: "amount_refunded"
                      <*> o .: "refunds"
                      <*> o .: "balance_transaction"
                      <*> o .: "account"
                      <*> (ApplicationId <$> o .: "application")
                      <*> o .: "charge"
                      <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `FeeId` for objects with Fees
newtype FeeId =
  FeeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Application Fee Refunds
data ApplicationFeeRefund = ApplicationFeeRefund {
       applicationFeeRefundId                 :: RefundId
     , applicationFeeRefundAmount             :: Int
     , applicationFeeRefundCurrency           :: Currency
     , applicationFeeRefundCreated            :: UTCTime
     , applicationFeeRefundObject             :: Text
     , applicationFeeRefundBalanceTransaction :: Maybe (Expandable TransactionId)
     , applicationFeeRefundFee                :: FeeId
     , applicationFeeRefundMetaData           :: MetaData
     } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFeeRefund`
instance FromJSON ApplicationFeeRefund where
    parseJSON (Object o) = ApplicationFeeRefund
              <$> (RefundId <$> o .: "id")
              <*> o .: "amount"
              <*> o .: "currency"
              <*> (fromSeconds <$> o .: "created")
              <*> o .: "object"
              <*> o .:? "balance_transaction"
              <*> (FeeId <$> o .: "fee")
              <*> o .: "metadata"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `AccountId` of an `Account`
newtype AccountId
  = AccountId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `AccountId`
instance FromJSON AccountId where
   parseJSON (String aid) = pure $ AccountId aid
   parseJSON _            = mzero

------------------------------------------------------------------------------
-- | `Account` Object
data Account = Account {
       accountId                  :: AccountId
     , accountEmail               :: Email
     , accountStatementDescriptor :: Maybe Description
     , accountDisplayName         :: Maybe Text
     , accountTimeZone            :: Text
     , accountDetailsSubmitted    :: Bool
     , accountChargeEnabled       :: Bool
     , accountTransferEnabled     :: Bool
     , accountCurrenciesSupported :: [Currency]
     , accountDefaultCurrency     :: Currency
     , accountCountry             :: Text
     , accountObject              :: Text
     , accountBusinessName        :: Maybe Text
     , accountBusinessURL         :: Maybe Text
     , accountBusinessLogo        :: Maybe Text
     , accountSupportPhone        :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Account`
instance FromJSON Account where
   parseJSON (Object o) =
       Account <$> (AccountId <$> o .:  "id")
               <*> (Email <$> o .:  "email")
               <*> o .:? "statement_descriptor"
               <*> o .:  "display_name"
               <*> o .:  "timezone"
               <*> o .:  "details_submitted"
               <*> o .:  "charge_enabled"
               <*> o .:  "transfer_enabled"
               <*> o .:  "currencies_supported"
               <*> o .:  "default_currency"
               <*> o .:  "country"
               <*> o .:  "object"
               <*> o .:?  "business_name"
               <*> o .:?  "business_url"
               <*> o .:?  "business_logo"
               <*> o .:?  "support_phone"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Balance` Object
data Balance = Balance {
      balancePending   :: [BalanceAmount]
    , balanceAvailable :: [BalanceAmount]
    , balanceLiveMode  :: Bool
    , balanceObject    :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Balance`
instance FromJSON Balance where
   parseJSON (Object o) =
       Balance <$> o .: "pending"
               <*> o .: "available"
               <*> o .: "livemode"
               <*> o .: "object"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BalanceAmount` Object
data BalanceAmount = BalanceAmount {
      balanceAmount   :: Int
    , balanceCurrency :: Currency
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceAmount`
instance FromJSON BalanceAmount where
   parseJSON (Object o) =
       BalanceAmount <$> o .: "amount"
                     <*> o .: "currency"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BalanceTransaction` Object
data BalanceTransaction = BalanceTransaction {
      balanceTransactionId             :: TransactionId
    , balanceTransactionObject         :: Text
    , balanceTransactionAmount         :: Int
    , balanceTransactionCurrency       :: Currency
    , balanceTransactionNet            :: Int
    , balanceTransactionType           :: TransactionType
    , balanceTransactionCreated        :: UTCTime
    , balanceTransactionAvailableOn    :: UTCTime
    , balanceTransactionStatus         :: Text
    , balanceTransactionFee            :: Int
    , balanceTransactionFeeDetails     :: [FeeDetails]
    , balanceTransactionFeeSource      :: Expandable ChargeId
    , balanceTransactionFeeDescription :: Maybe Description
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceTransaction`
instance FromJSON BalanceTransaction where
   parseJSON (Object o) =
       BalanceTransaction <$> (TransactionId <$> o .: "id")
                          <*> o .: "object"
                          <*> o .: "amount"
                          <*> o .: "currency"
                          <*> o .: "net"
                          <*> o .: "type"
                          <*> (fromSeconds <$> o .: "created")
                          <*> (fromSeconds <$> o .: "available_on")
                          <*> o .: "status"
                          <*> o .: "fee"
                          <*> o .: "fee_details"
                          <*> o .: "source"
                          <*> o .:? "description"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `FeeDetails` Object
data FeeDetails = FeeDetails {
      feeDetailsAmount   :: Int
    , feeDetailsCurrency :: Currency
    , feeType            :: Text
    , feeDescription     :: Maybe Description
    , feeApplication     :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `FeeDetails`
instance FromJSON FeeDetails where
   parseJSON (Object o) =
       FeeDetails <$> o .: "amount"
                  <*> o .: "currency"
                  <*> o .: "type"
                  <*> o .: "description"
                  <*> o .:? "application"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | EventData
data EventData =
    TransferEvent Transfer
  | AccountEvent Account
  | AccountApplicationEvent ConnectApp
  | ApplicationFeeEvent ApplicationFee
  | InvoiceEvent Invoice
  | PlanEvent Plan
  | RecipientEvent Recipient
  | CouponEvent Coupon
  | BalanceEvent Balance
  | ChargeEvent Charge
  | DisputeEvent Dispute
  | CustomerEvent Customer
  | CardEvent Card
  | SubscriptionEvent Subscription
  | DiscountEvent Discount
  | InvoiceItemEvent InvoiceItem
  | UnknownEventData
  | Ping
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Event` Object
data Event = Event {
      eventId              :: Maybe EventId
    , eventCreated         :: UTCTime
    , eventLiveMode        :: Bool
    , eventType            :: EventType
    , eventData            :: EventData
    , eventObject          :: Text
    , eventPendingWebHooks :: Int
    , eventRequest         :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Event`
instance FromJSON Event where
   parseJSON (Object o) = do
     eventId <- fmap EventId <$> o .:? "id"
     eventCreated <- fromSeconds <$> o .: "created"
     eventLiveMode <- o .: "livemode"
     eventType <- o .: "type"
     String etype <- o .: "type"
     obj <- o .: "data"
     eventData <-
       case etype of
        "account.updated" -> AccountEvent <$> obj .: "object"
        "account.application.deauthorized" -> AccountApplicationEvent <$> obj .: "object"
        "application_fee.created" -> ApplicationFeeEvent <$> obj .: "object"
        "application_fee.refunded" -> ApplicationFeeEvent <$> obj .: "object"
        "balance.available" -> BalanceEvent <$> obj .: "object"
        "charge.succeeded" -> ChargeEvent <$> obj .: "object"
        "charge.failed" -> ChargeEvent <$> obj .: "object"
        "charge.refunded" -> ChargeEvent <$> obj .: "object"
        "charge.captured" -> ChargeEvent <$> obj .: "object"
        "charge.updated" -> ChargeEvent <$> obj .: "object"
        "charge.dispute.created" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.updated" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.closed" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_withdrawn" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_reinstated" -> DisputeEvent <$> obj .: "object"
        "customer.created" -> CustomerEvent <$> obj .: "object"
        "customer.updated" -> CustomerEvent <$> obj .: "object"
        "customer.deleted" -> CustomerEvent <$> obj .: "object"
        "customer.card.created" -> CardEvent <$> obj .: "object"
        "customer.card.updated" -> CardEvent <$> obj .: "object"
        "customer.card.deleted" -> CardEvent <$> obj .: "object"
        "customer.subscription.created" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.updated" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.deleted" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.trial_will_end" -> SubscriptionEvent <$> obj .: "object"
        "customer.discount.created" -> DiscountEvent <$> obj .: "object"
        "customer.discount.updated" -> DiscountEvent <$> obj .: "object"
        "customer.discount.deleted" -> DiscountEvent <$> obj .: "object"
        "invoice.created" -> InvoiceEvent <$> obj .: "object"
        "invoice.updated" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_succeeded" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_failed" -> InvoiceEvent <$> obj .: "object"
        "invoiceitem.created" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.updated" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.deleted" -> InvoiceItemEvent <$> obj .: "object"
        "plan.created" -> PlanEvent <$> obj .: "object"
        "plan.updated" -> PlanEvent <$> obj .: "object"
        "plan.deleted" -> PlanEvent <$> obj .: "object"
        "coupon.created" -> CouponEvent <$> obj .: "object"
        "coupon.updated" -> CouponEvent <$> obj .: "object"
        "coupon.deleted" -> CouponEvent <$> obj .: "object"
        "recipient.created" -> RecipientEvent <$> obj .: "object"
        "recipient.updated" -> RecipientEvent <$> obj .: "object"
        "recipient.deleted" -> RecipientEvent <$> obj .: "object"
        "transfer.created" -> TransferEvent <$> obj .: "object"
        "transfer.updated" -> TransferEvent <$> obj .: "object"
        "transfer.canceled" -> TransferEvent <$> obj .: "object"
        "transfer.paid" -> TransferEvent <$> obj .: "object"
        "transfer.failed" -> TransferEvent <$> obj .: "object"
        "ping" -> pure Ping
        _        -> pure UnknownEventData
     eventObject <- o .: "object"
     eventPendingWebHooks <- o .: "pending_webhooks"
     eventRequest <- o .:? "request"
     return Event {..}
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Connect Application
data ConnectApp = ConnectApp {
      connectAppId     :: Maybe Text
    , connectAppObject :: Text
    , connectAppName   :: Text
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Connect Application JSON instance
instance FromJSON ConnectApp where
   parseJSON (Object o) =
     ConnectApp <$> o .:? "id"
                <*> o .: "object"
                <*> o .: "name"
   parseJSON _  = mzero

------------------------------------------------------------------------------
-- | `Token` Object
data Token a = Token {
      tokenId       :: TokenId
    , tokenLiveMode :: Bool
    , tokenCreated  :: UTCTime
    , tokenUsed     :: Bool
    , tokenObject   :: Text
    , tokenType     :: TokenType
    , tokenData     :: a
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Token`
instance FromJSON a => FromJSON (Token a) where
   parseJSON (Object o) = do
     tokenId <- TokenId <$> o .: "id"
     Bool tokenLiveMode <- o .: "livemode"
     tokenCreated <- fromSeconds <$> o .: "created"
     Bool tokenUsed <- o .: "used"
     String tokenObject <- o .: "object"
     String typ <- o .: "type"
     tokenType <- pure $ case typ of
                      "card"         -> TokenCard
                      "bank_account" -> TokenBankAccount
                      _              -> error "unspecified type"
     tokenData <-
       case typ of
        "bank_account" -> o .: "bank_account"
        "card"         -> o .: "card"
        _              -> mzero
     return Token {..}
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON returned from a `Stripe` deletion request
data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeDeleteResult`
instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .:? "id"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Type of Expansion Parameters for use on `Stripe` objects
newtype ExpandParams = ExpandParams { getExpandParams :: [Text] }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ExpandParams where
  toStripeParam (ExpandParams params) =
    (toExpandable params ++)

------------------------------------------------------------------------------
-- | Generic ID for use in constructing API Calls
type ID    = Text

------------------------------------------------------------------------------
-- | Generic URL for use in constructing API Calls
type URL   = Text

------------------------------------------------------------------------------
-- | BTC ReceiverObject
data BitcoinReceiver = BitcoinReceiver {
       btcId                    :: BitcoinReceiverId
    ,  btcObject                :: Text
    ,  btcCreated               :: UTCTime
    ,  btcLiveMode              :: Bool
    ,  btcActive                :: Bool
    ,  btcAmount                :: Integer
    ,  btcAmountReceived        :: Integer
    ,  btcBitcoinAmount         :: Integer
    ,  btcBitcoinAmountReceived :: Integer
    ,  btcBitcoinUri            :: Text
    ,  btcCurrency              :: Currency
    ,  btcFilled                :: Bool
    ,  btcInboundAddress        :: Text
    ,  btcUncapturedFunds       :: Bool
    ,  btcDescription           :: Maybe Text
    ,  btcEmail                 :: Text
    ,  btcMetadata              :: MetaData
    ,  btcRefundAddress         :: Maybe Text
    ,  btcTransactions          :: Maybe Transactions
    ,  btcPayment               :: Maybe PaymentId
    ,  btcCustomer              :: Maybe CustomerId
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for BitcoinReceiverId
instance FromJSON BitcoinReceiver where
   parseJSON (Object o) =
     BitcoinReceiver <$> (BitcoinReceiverId <$> o .: "id")
                     <*> o .: "object"
                     <*> (fromSeconds <$> o .: "created")
                     <*> o .: "livemode"
                     <*> o .: "active"
                     <*> o .: "amount"
                     <*> o .: "amount_received"
                     <*> o .: "bitcoin_amount"
                     <*> o .: "bitcoin_amount_received"
                     <*> o .: "bitcoin_uri"
                     <*> o .: "currency"
                     <*> o .: "filled"
                     <*> o .: "inbound_address"
                     <*> o .: "uncaptured_funds"
                     <*> o .:? "description"
                     <*> o .: "email"
                     <*> (MetaData . H.toList <$> o .: "metadata")
                     <*> o .:? "refund_address"
                     <*> o .:? "transactions"
                     <*> (fmap PaymentId <$> o .:? "payment")
                     <*> (fmap CustomerId <$> o .:? "customer")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Bitcoin Transactions
data Transactions = Transactions {
      transactionsObject     :: Text
    , transactionsTotalCount :: Integer
    , transactionsHasMore    :: Bool
    , transactionsURL        :: Text
    , transactions           :: [BitcoinTransaction]
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Bitcoin Transactions data
instance FromJSON Transactions where
   parseJSON (Object o) =
     Transactions <$> o .: "object"
                  <*> o .: "total_count"
                  <*> o .: "has_more"
                  <*> o .: "url"
                  <*> o .: "data"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Bitcoin Transaction
data BitcoinTransaction = BitcoinTransaction {
         btcTransactionId            :: BitcoinTransactionId
       , btcTransactionObject        :: Text
       , btcTransactionCreated       :: UTCTime
       , btcTransactionAmount        :: Integer
       , btcTransactionBitcoinAmount :: Integer
       , btcTransactionCurrency      :: Currency
       , btcTransactionReceiver      :: BitcoinReceiverId
      } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransaction
instance FromJSON BitcoinTransaction where
   parseJSON (Object o) =
     BitcoinTransaction <$> o .: "id"
                        <*> o .: "object"
                        <*> (fromSeconds <$> o .: "created")
                        <*> o .: "amount"
                        <*> o .: "bitcoin_amount"
                        <*> o .: "currency"
                        <*> o .: "receiver"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | BitcoinTransactionId
newtype BitcoinTransactionId =
    BitcoinTransactionId Text
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransactionId
instance FromJSON BitcoinTransactionId where
   parseJSON (String o) = pure $ BitcoinTransactionId o
   parseJSON _          = mzero

------------------------------------------------------------------------------
-- | BTC ReceiverId
newtype BitcoinReceiverId = BitcoinReceiverId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for BitcoinReceiverId
instance FromJSON BitcoinReceiverId where
   parseJSON (String x) = pure $ BitcoinReceiverId x
   parseJSON _          = mzero

------------------------------------------------------------------------------
-- | BTC PaymentId
newtype PaymentId = PaymentId Text
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | FromJSON for PaymentId
instance FromJSON PaymentId where
   parseJSON (String x) = pure $ PaymentId x
   parseJSON _          = mzero
