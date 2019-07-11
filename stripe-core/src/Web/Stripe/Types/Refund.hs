{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Refund where

import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))


------------------------------------------------------------------------------
-- | `RefundId` for `Refund`
newtype RefundId =
  RefundId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam RefundId where
  toStripeParam (RefundId fid) =
    (("refund", Text.encodeUtf8 fid) :)

------------------------------------------------------------------------------
-- | `RefundApplicationFee`
newtype RefundApplicationFee =
  RefundApplicationFee { getRefundApplicationFee :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam RefundApplicationFee where
  toStripeParam (RefundApplicationFee b) =
    (("refund_application_fee", if b then "true" else "false") :)

------------------------------------------------------------------------------
-- | `RefundReason`
data RefundReason
  = RefundDuplicate
  | RefundFraudulent
  | RefundRequestedByCustomer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam RefundReason where
  toStripeParam reason =
    (("reason", case reason of
         RefundDuplicate           -> "duplicate"
         RefundFraudulent          -> "fraudulent"
         RefundRequestedByCustomer -> "requested_by_customer") :)
