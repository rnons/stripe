{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Email where

import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `Email` associated with a `Customer`, `Recipient` or `Charge`
newtype Email = Email Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Email where
  toStripeParam (Email txt) =
    (("email", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | `Email` to send receipt to
newtype ReceiptEmail = ReceiptEmail Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ReceiptEmail where
  toStripeParam (ReceiptEmail txt) =
    (("receipt_email", Text.encodeUtf8 txt) :)
