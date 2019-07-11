{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Event where

import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `EventId` of an `Event`
newtype EventId = EventId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam EventId where
  toStripeParam (EventId eid) =
    (("event", Text.encodeUtf8 eid) :)

------------------------------------------------------------------------------
-- | `Event` Types
data EventType =
    AccountUpdatedEvent
  | AccountApplicationDeauthorizedEvent
  | ApplicationFeeCreatedEvent
  | ApplicationFeeRefundedEvent
  | BalanceAvailableEvent
  | ChargeSucceededEvent
  | ChargeFailedEvent
  | ChargeRefundedEvent
  | ChargeCapturedEvent
  | ChargeUpdatedEvent
  | ChargeDisputeCreatedEvent
  | ChargeDisputeUpdatedEvent
  | ChargeDisputeClosedEvent
  | ChargeDisputeFundsWithdrawnEvent
  | ChargeDisputeFundsReinstatedEvent
  | CustomerCreatedEvent
  | CustomerUpdatedEvent
  | CustomerDeletedEvent
  | CustomerCardCreatedEvent
  | CustomerCardUpdatedEvent
  | CustomerCardDeletedEvent
  | CustomerSubscriptionCreatedEvent
  | CustomerSubscriptionUpdatedEvent
  | CustomerSubscriptionDeletedEvent
  | CustomerSubscriptionTrialWillEndEvent
  | CustomerDiscountCreatedEvent
  | CustomerDiscountUpdatedEvent
  | CustomerDiscountDeletedEvent
  | InvoiceCreatedEvent
  | InvoiceUpdatedEvent
  | InvoicePaymentSucceededEvent
  | InvoicePaymentFailedEvent
  | InvoiceItemCreatedEvent
  | InvoiceItemUpdatedEvent
  | InvoiceItemDeletedEvent
  | PlanCreatedEvent
  | PlanUpdatedEvent
  | PlanDeletedEvent
  | CouponCreatedEvent
  | CouponUpdatedEvent
  | CouponDeletedEvent
  | RecipientCreatedEvent
  | RecipientUpdatedEvent
  | RecipientDeletedEvent
  | TransferCreatedEvent
  | TransferUpdatedEvent
  | TransferCanceledEvent
  | TransferPaidEvent
  | TransferFailedEvent
  | PingEvent
  | UnknownEvent
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Event Types JSON Instance
instance FromJSON EventType where
   parseJSON (String "account.updated") = pure AccountUpdatedEvent
   parseJSON (String "account.application.deauthorized") = pure AccountApplicationDeauthorizedEvent
   parseJSON (String "application_fee.created") = pure ApplicationFeeCreatedEvent
   parseJSON (String "application_fee.refunded") = pure ApplicationFeeRefundedEvent
   parseJSON (String "balance.available") = pure BalanceAvailableEvent
   parseJSON (String "charge.succeeded") = pure ChargeSucceededEvent
   parseJSON (String "charge.failed") = pure ChargeFailedEvent
   parseJSON (String "charge.refunded") = pure ChargeRefundedEvent
   parseJSON (String "charge.captured") = pure ChargeCapturedEvent
   parseJSON (String "charge.updated") = pure ChargeUpdatedEvent
   parseJSON (String "charge.dispute.created") = pure ChargeDisputeCreatedEvent
   parseJSON (String "charge.dispute.updated") = pure ChargeDisputeUpdatedEvent
   parseJSON (String "charge.dispute.closed") = pure ChargeDisputeClosedEvent
   parseJSON (String "charge.dispute.funds_withdrawn") = pure ChargeDisputeFundsWithdrawnEvent
   parseJSON (String "charge.dispute.funds_reinstated") = pure ChargeDisputeFundsReinstatedEvent
   parseJSON (String "customer.created") = pure CustomerCreatedEvent
   parseJSON (String "customer.updated") = pure CustomerUpdatedEvent
   parseJSON (String "customer.deleted") = pure CustomerDeletedEvent
   parseJSON (String "customer.card.created") = pure CustomerCardCreatedEvent
   parseJSON (String "customer.card.updated") = pure CustomerCardUpdatedEvent
   parseJSON (String "customer.card.deleted") = pure CustomerCardDeletedEvent
   parseJSON (String "customer.subscription.created") = pure CustomerSubscriptionCreatedEvent
   parseJSON (String "customer.subscription.updated") = pure CustomerSubscriptionUpdatedEvent
   parseJSON (String "customer.subscription.deleted") = pure CustomerSubscriptionDeletedEvent
   parseJSON (String "customer.subscription.trial_will_end") = pure CustomerSubscriptionTrialWillEndEvent
   parseJSON (String "customer.discount.created") = pure CustomerDiscountCreatedEvent
   parseJSON (String "customer.discount.updated") = pure CustomerDiscountUpdatedEvent
   parseJSON (String "invoice.created") = pure InvoiceCreatedEvent
   parseJSON (String "invoice.updated") = pure InvoiceUpdatedEvent
   parseJSON (String "invoice.payment_succeeded") = pure InvoicePaymentSucceededEvent
   parseJSON (String "invoice.payment_failed") = pure InvoicePaymentFailedEvent
   parseJSON (String "invoiceitem.created") = pure InvoiceItemCreatedEvent
   parseJSON (String "invoiceitem.updated") = pure InvoiceItemUpdatedEvent
   parseJSON (String "invoiceitem.deleted") = pure InvoiceItemDeletedEvent
   parseJSON (String "plan.created") = pure PlanCreatedEvent
   parseJSON (String "plan.updated") = pure PlanUpdatedEvent
   parseJSON (String "plan.deleted") = pure PlanDeletedEvent
   parseJSON (String "coupon.created") = pure CouponCreatedEvent
   parseJSON (String "coupon.updated") = pure CouponUpdatedEvent
   parseJSON (String "coupon.deleted") = pure CouponDeletedEvent
   parseJSON (String "recipient.created") = pure RecipientCreatedEvent
   parseJSON (String "recipient.updated") = pure RecipientUpdatedEvent
   parseJSON (String "recipient.deleted") = pure RecipientDeletedEvent
   parseJSON (String "transfer.created") = pure TransferCreatedEvent
   parseJSON (String "transfer.updated") = pure TransferUpdatedEvent
   parseJSON (String "transfer.canceled") = pure TransferCanceledEvent
   parseJSON (String "transfer.paid") = pure TransferPaidEvent
   parseJSON (String "transfer.failed") = pure TransferFailedEvent
   parseJSON (String "ping") = pure PingEvent
   parseJSON _ = pure UnknownEvent
