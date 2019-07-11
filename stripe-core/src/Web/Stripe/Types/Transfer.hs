{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Transfer where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | `TransferId`
newtype TransferId =
  TransferId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TransferId where
  toStripeParam (TransferId tid) =
    (("transfer", Text.encodeUtf8 tid) :)

------------------------------------------------------------------------------
-- | Status of a `Transfer`
data TransferStatus =
    TransferPaid
  | TransferPending
  | TransferCanceled
  | TransferFailed
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TransferStatus where
  toStripeParam transferStatus =
    (("status", toBytestring transferStatus) :)

------------------------------------------------------------------------------
-- | Type of a `Transfer`
data TransferType =
    CardTransfer
  | BankAccountTransfer
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransferType`
instance FromJSON TransferType where
    parseJSON (String "card")         = pure CardTransfer
    parseJSON (String "bank_account") = pure BankAccountTransfer
    parseJSON _                       = mzero

------------------------------------------------------------------------------
-- | JSON Instance for `TransferStatus`
instance FromJSON TransferStatus where
    parseJSON (String "paid")     = pure TransferPaid
    parseJSON (String "pending")  = pure TransferPending
    parseJSON (String "canceled") = pure TransferCanceled
    parseJSON _                   = mzero
