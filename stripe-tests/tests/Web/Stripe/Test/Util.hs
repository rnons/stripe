module Web.Stripe.Test.Util
       ( -- * Helpers
         makePlanId
       , makeSubscriptionId
       , makeCouponId
       , secs
       ) where

import           Control.Monad
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.Random

import           Web.Stripe.Coupon
import           Web.Stripe.Plan
import           Web.Stripe.Subscription

------------------------------------------------------------------------------
-- | `PlanId` creation helper
makePlanId :: IO PlanId
makePlanId = PlanId <$> makeGuid

------------------------------------------------------------------------------
-- | `SubscriptionId` creation helper
makeSubscriptionId :: IO SubscriptionId
makeSubscriptionId = SubscriptionId <$> makeGuid

------------------------------------------------------------------------------
-- | `CouponId` creation helper
makeCouponId :: IO CouponId
makeCouponId = CouponId <$> makeGuid

------------------------------------------------------------------------------
-- | Guid Creation Helper
makeGuid :: IO Text
makeGuid = T.pack <$> replicateM 10 (randomRIO ('a', 'z'))

------------------------------------------------------------------------------
-- | Seconds
secs :: Int -> Int
secs = (*1000000)
