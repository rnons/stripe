{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Plan where

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
-- | `PlanId` for a `Plan`
newtype PlanId = PlanId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam PlanId where
  toStripeParam (PlanId pid) =
    (("plan", Text.encodeUtf8 pid) :)

------------------------------------------------------------------------------
-- | a plan name
newtype PlanName  = PlanName { getPlanName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON PlanName where
  parseJSON v = PlanName <$> parseJSON v

instance ToStripeParam PlanName where
  toStripeParam (PlanName txt) =
    (("name", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Interval for `Plan`s
data Interval = Day | Week | Month | Year deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Interval`
instance FromJSON Interval where
   parseJSON (String "day")   = pure Day
   parseJSON (String "week")  = pure Week
   parseJSON (String "month") = pure Month
   parseJSON (String "year")  = pure Year
   parseJSON _                = mzero

------------------------------------------------------------------------------
-- | `Show` instance for `Interval`
instance Show Interval where
    show Day   = "day"
    show Week  = "week"
    show Month = "month"
    show Year  = "year"

------------------------------------------------------------------------------
-- | `Read` instance for `Interval`
instance Read Interval where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "day"   -> return Day
         "week"  -> return Week
         "month" -> return Month
         "year"  -> return Year
         _       -> pfail

instance ToStripeParam Interval where
  toStripeParam interval =
    (("interval", toBytestring interval) :)

------------------------------------------------------------------------------
-- | `TrialEnd` for a Plan
newtype TrialEnd = TrialEnd UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TrialEnd where
  toStripeParam (TrialEnd time) =
    (("trial_end", toBytestring $ toSeconds time) :)
