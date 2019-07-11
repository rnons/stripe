{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Transaction where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 ToJSON (toJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `TransactionId` of a `Transaction`
newtype TransactionId = TransactionId Text
                   deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransactionId`
instance FromJSON TransactionId where
    parseJSON (String x) = pure (TransactionId x)
    parseJSON _          = mzero

instance ToStripeParam TransactionId where
  toStripeParam (TransactionId tid) =
    (("transaction", Text.encodeUtf8 tid) :)

------------------------------------------------------------------------------
-- | transaction type for `BalanceTransaction`
data TransactionType
  = ChargeTxn
  | RefundTxn
  | AdjustmentTxn
  | ApplicationFeeTxn
  | ApplicationFeeRefundTxn
  | TransferTxn
  | TransferCancelTxn
  | TransferFailureTxn
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TransactionType where
  parseJSON (String "charge")                 = pure ChargeTxn
  parseJSON (String "refund")                 = pure RefundTxn
  parseJSON (String "adjustment")             = pure AdjustmentTxn
  parseJSON (String "application_fee")        = pure ApplicationFeeTxn
  parseJSON (String "application_fee_refund") = pure ApplicationFeeRefundTxn
  parseJSON (String "transfer")               = pure TransferTxn
  parseJSON (String "transfer_cancel")        = pure TransferCancelTxn
  parseJSON (String "transfer_failure")       = pure TransferFailureTxn
  parseJSON _                                 = mzero

instance ToJSON TransactionType where
  toJSON ChargeTxn               = String "charge"
  toJSON RefundTxn               = String "refund"
  toJSON AdjustmentTxn           = String "adjustment"
  toJSON ApplicationFeeTxn       = String "application_fee"
  toJSON ApplicationFeeRefundTxn = String "application_fee_refund"
  toJSON TransferTxn             = String "transfer"
  toJSON TransferCancelTxn       = String "transfer_cancel"
  toJSON TransferFailureTxn      = String "transfer_failure"

instance ToStripeParam TransactionType where
  toStripeParam txn =
    (("type", case txn of
                ChargeTxn          -> "charge"
                RefundTxn          -> "refund"
                AdjustmentTxn      -> "adjustment"
                ApplicationFeeTxn  -> "application_fee"
                ApplicationFeeRefundTxn
                                   -> "application_fee_refund"
                TransferTxn        -> "transfer"
                TransferCancelTxn  -> "transfer_cancel"
                TransferFailureTxn -> "transfer_failure") :)
