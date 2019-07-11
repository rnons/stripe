{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Charge where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.String                    (fromString)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Numeric                        (showFFloat)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | Amount representing a monetary value.
-- Stripe represents pennies as whole numbers
-- i.e. 100 = $1
newtype Amount = Amount { getAmount :: Int }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Amount where
  toStripeParam (Amount i) =
    (("amount", toBytestring i) :)

------------------------------------------------------------------------------
newtype ApplicationFeeId = ApplicationFeeId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ApplicationFeeId where
  toStripeParam (ApplicationFeeId aid) =
    (("application_fee", Text.encodeUtf8 aid) :)

------------------------------------------------------------------------------
-- | ApplicationFeePercent
newtype ApplicationFeePercent = ApplicationFeePercent Double
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ApplicationFeePercent where
  toStripeParam (ApplicationFeePercent fee) =
    (("application_fee_percent", fromString $ showFFloat (Just 2) fee "") :)

------------------------------------------------------------------------------
-- | ApplicationFeeAmount
newtype ApplicationFeeAmount = ApplicationFeeAmount Integer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ApplicationFeeAmount where
  toStripeParam (ApplicationFeeAmount cents) =
    (("application_fee", toBytestring cents) :)

------------------------------------------------------------------------------
-- | `ApplicationId` object
newtype ApplicationId =
  ApplicationId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Capture for `Charge`
newtype Capture = Capture { getCapture :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Capture where
  toStripeParam (Capture b) =
    (("capture", if b then "true" else "false") :)

------------------------------------------------------------------------------
-- | `ChargeId` associated with a `Charge`
newtype ChargeId
  = ChargeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ChargeId`
instance FromJSON ChargeId where
   parseJSON (String x) = pure $ ChargeId x
   parseJSON _          = mzero

instance ToStripeParam ChargeId where
  toStripeParam (ChargeId cid) =
    (("charge", Text.encodeUtf8 cid) :)
