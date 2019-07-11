{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Coupon where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Time                      (UTCTime)
import           Text.Read                      (lexP, pfail)
import qualified Text.Read                      as R
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring, toSeconds)

------------------------------------------------------------------------------
-- | `AmountOff` for a `Coupon`
newtype AmountOff = AmountOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AmountOff where
  toStripeParam (AmountOff i) =
    (("amount_off", toBytestring i) :)

------------------------------------------------------------------------------
-- | `CouponId` for a `Coupon`
newtype CouponId = CouponId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam CouponId where
  toStripeParam (CouponId cid) =
    (("coupon", Text.encodeUtf8 cid) :)

------------------------------------------------------------------------------
-- | `Coupon` Duration
data Duration = Forever | Once | Repeating deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Show` instance for `Duration`
instance Show Duration where
    show Forever   = "forever"
    show Once      = "once"
    show Repeating = "repeating"

------------------------------------------------------------------------------
-- | `Read` instance for `Duration`
instance Read Duration where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "forever"   -> return Forever
         "once"      -> return Once
         "repeating" -> return Repeating
         _           -> pfail

------------------------------------------------------------------------------
-- | JSON Instance for `Duration`
instance FromJSON Duration where
   parseJSON (String x)
       | x == "forever"   = pure Forever
       | x == "once"      = pure Once
       | x == "repeating" = pure Repeating
   parseJSON _ = mzero

instance ToStripeParam Duration where
  toStripeParam duration =
    (("duration", toBytestring duration) :)

------------------------------------------------------------------------------
-- | `MaxRedemptions` for a `Coupon`
newtype MaxRedemptions = MaxRedemptions Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam MaxRedemptions where
  toStripeParam (MaxRedemptions i) =
    (("max_redemptions", toBytestring i) :)

------------------------------------------------------------------------------
-- | `PercentOff` for a `Coupon`
newtype PercentOff = PercentOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam PercentOff where
  toStripeParam (PercentOff i) =
    (("percent_off", toBytestring i) :)

------------------------------------------------------------------------------
-- | `RedeemBy` date for a `Coupon`
newtype RedeemBy = RedeemBy UTCTime deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam RedeemBy where
  toStripeParam (RedeemBy time) =
    (("redeem_by", toBytestring $ toSeconds time) :)

------------------------------------------------------------------------------
-- | `DurationInMonths` for a `Coupon`
newtype DurationInMonths = DurationInMonths Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam DurationInMonths where
  toStripeParam (DurationInMonths i) =
    (("duration_in_months", toBytestring i) :)

------------------------------------------------------------------------------
-- | `IntervalCount` for a `Coupon`
newtype IntervalCount   = IntervalCount Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam IntervalCount where
  toStripeParam (IntervalCount c) =
    (("interval_count", toBytestring c) :)

------------------------------------------------------------------------------
-- | `TrialPeriodDays` for a `Coupon`
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TrialPeriodDays where
  toStripeParam (TrialPeriodDays days) =
    (("trial_period_days", toBytestring days) :)
