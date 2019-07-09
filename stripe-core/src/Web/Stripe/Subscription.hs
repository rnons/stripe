{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Subscription
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#subscriptions >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Subscription
-- import Web.Stripe.Customer
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ createCustomer
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) -> do
--       result <- stripe config $ createPlan (PlanId "free plan")
--                                            (Amount 0)
--                                            USD
--                                            Month
--                                            (PlanName "sample plan")
--       case result of
--         (Left stripeError) -> print stripeError
--         (Right (Plan { planId = pid })) -> do
--            result <- stripe config $ createSubscription cid pid
--            case result of
--              (Left stripeError)   -> print stripeError
--              (Right subscription) -> print subscription
-- @
module Web.Stripe.Subscription
    ( -- * API
      CreateSubscription
    , createSubscription
    , CreateSubscriptionSubscriptionItem(..)
    , GetSubscription
    , getSubscription
    , UpdateSubscription
    , updateSubscription
    , CancelSubscription
    , cancelSubscription
    , GetSubscriptions
    , getSubscriptions
    , GetSubscriptionsByCustomerId
    , getSubscriptionsByCustomerId
      -- * Types
    , ApplicationFeePercent (..)
    , AtPeriodEnd        (..)
    , CustomerId         (..)
    , CouponId           (..)
    , Coupon             (..)
    , EndingBefore       (..)
    , ExpandParams       (..)
    , Limit              (..)
    , MetaData           (..)
    , PlanId             (..)
    , Prorate            (..)
    , Quantity           (..)
    , StartingAfter      (..)
    , StripeList         (..)
    , Subscription       (..)
    , SubscriptionId     (..)
    , SubscriptionStatus (..)
    , TaxPercent         (..)
    , TrialEnd           (..)
    ) where

import           Control.Monad            (join)
import qualified Data.ByteString.Char8    as B8
import qualified Data.Text                as T
import           Web.Stripe.StripeRequest (Method (DELETE, GET, POST),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam (..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (ApplicationFeePercent (..),
                                           AtPeriodEnd (..), BillingCycleAnchor,
                                           CardId (..), Coupon (..),
                                           CouponId (..), CustomerId (..),
                                           EndingBefore (..), ExpandParams (..),
                                           Limit (..), MetaData (..),
                                           PlanId (..), Prorate (..),
                                           Quantity (..), StartingAfter (..),
                                           StripeList (..), Subscription (..),
                                           SubscriptionCollectionMethod,
                                           SubscriptionId (..),
                                           SubscriptionStatus (..),
                                           TaxPercent (..), TaxRateId (..),
                                           TrialEnd (..))
import           Web.Stripe.Types.Util    (getCustomerId)
import           Web.Stripe.Util          ((</>))

------------------------------------------------------------------------------
-- | Create a `Subscription` by `CustomerId` and `PlanId`
createSubscription
    :: CustomerId -- ^ The `CustomerId` upon which to create the `Subscription`
    -> [CreateSubscriptionSubscriptionItem]     -- ^ The `SubscriptionItem` list
    -> StripeRequest CreateSubscription
createSubscription
    customerId
    items = request
  where request = mkStripeRequest POST url params
        url     = "subscriptions"
        params =
            toStripeParam customerId $
            toStripeParam items []

data CreateSubscription
type instance StripeReturn CreateSubscription = Subscription
instance StripeHasParam CreateSubscription ApplicationFeePercent
instance StripeHasParam CreateSubscription BillingCycleAnchor
-- instance StripeHasParam CreateSubscription BillingThresholds
instance StripeHasParam CreateSubscription SubscriptionCollectionMethod
instance StripeHasParam CreateSubscription CouponId
-- instance StripeHasParam CreateSubscription DaysUntilDue
-- instance StripeHasParam CreateSubscription DefaultPaymentMethod
-- instance StripeHasParam CreateSubscription DefaultSource
-- instance StripeHasParam CreateSubscription DefaultTaxRates
instance StripeHasParam CreateSubscription MetaData
instance StripeHasParam CreateSubscription Prorate
instance StripeHasParam CreateSubscription TrialEnd
-- instance StripeHasParam CreateSubscription TrialFromPlan
-- instance StripeHasParam CreateSubscription TrialPeriodDays

------------------------------------------------------------------------------
-- | The `SubscriptionItem` when creating a `Subscription`
data CreateSubscriptionSubscriptionItem = CreateSubscriptionSubscriptionItem
    { plan     :: PlanId
    -- , billingThresholds
    , metaData :: Maybe MetaData
    , quantity :: Maybe Int
    , taxRates :: Maybe TaxRateId
    }

instance ToStripeParam [CreateSubscriptionSubscriptionItem] where
    toStripeParam items xs = xs <> join (map
        (\(CreateSubscriptionSubscriptionItem{..}, index) ->
            let item = "items[" <> B8.pack (show index) <> "]"
                metaDataParam = case metaData of
                    Nothing -> []
                    Just _  -> []
                quantityParam = case quantity of
                    Nothing -> []
                    Just q ->
                        [ ( item <> "[quantity]"
                          , B8.pack $ show q
                          )
                        ]
                rateParam = case taxRates of
                    Nothing -> []
                    Just rateId ->
                        [ ( item <> "[tax_rates]"
                          , B8.pack $ T.unpack $ (\(TaxRateId x) -> x) rateId
                          )
                        ]
            in  [ (item <> "[plan]", B8.pack $ T.unpack $ (\(PlanId x) -> x) plan)
                ] <> metaDataParam <> quantityParam <> rateParam
        ) (zip items ([0,1..] :: [Int]))
        )

------------------------------------------------------------------------------
-- | Retrieve a `Subscription` by `CustomerId` and `SubscriptionId`
getSubscription
    :: CustomerId       -- ^ The `CustomerId` of the `Subscription`
    -> SubscriptionId   -- ^ The `SubscriptionId` of the `Subscription` to retrieve
    -> StripeRequest GetSubscription
getSubscription
    customerid
    (SubscriptionId subscriptionid)
                = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </>
                  "subscriptions" </> subscriptionid
        params  = []

data GetSubscription
type instance StripeReturn GetSubscription = Subscription
instance StripeHasParam GetSubscription ExpandParams

------------------------------------------------------------------------------
-- | Update a `Subscription` by `CustomerId` and `SubscriptionId`
updateSubscription
    :: CustomerId      -- ^ The `CustomerId` of the `Subscription` to update
    -> SubscriptionId  -- ^ The `SubscriptionId` of the `Subscription` to update
    -> StripeRequest UpdateSubscription
updateSubscription
   customerid
    (SubscriptionId subscriptionid)
  = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = []

data UpdateSubscription
type instance StripeReturn UpdateSubscription = Subscription
instance StripeHasParam UpdateSubscription PlanId
instance StripeHasParam UpdateSubscription CouponId
instance StripeHasParam UpdateSubscription Prorate
instance StripeHasParam UpdateSubscription TrialEnd
instance StripeHasParam UpdateSubscription CardId
instance StripeHasParam UpdateSubscription Quantity
instance StripeHasParam UpdateSubscription ApplicationFeePercent
instance StripeHasParam UpdateSubscription MetaData
instance StripeHasParam UpdateSubscription TaxPercent

------------------------------------------------------------------------------
-- | Delete a `Subscription` by `CustomerId` and `SubscriptionId`
cancelSubscription
    :: CustomerId     -- ^ The `CustomerId` of the `Subscription` to cancel
    -> SubscriptionId -- ^ The `SubscriptionId` of the `Subscription` to cancel
    -> StripeRequest CancelSubscription
cancelSubscription
    customerid
    (SubscriptionId subscriptionid)
     = request
  where request = mkStripeRequest DELETE url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions" </> subscriptionid
        params  = []

data CancelSubscription
type instance StripeReturn CancelSubscription = Subscription

------------------------------------------------------------------------------
-- | Retrieve all active `Subscription`s
getSubscriptions
    :: StripeRequest GetSubscriptions
getSubscriptions
    = request
  where request = mkStripeRequest GET url params
        url     = "subscriptions"
        params  = []

data GetSubscriptions
type instance StripeReturn GetSubscriptions = StripeList Subscription
instance StripeHasParam GetSubscriptions ExpandParams
instance StripeHasParam GetSubscriptions (EndingBefore SubscriptionId)
instance StripeHasParam GetSubscriptions Limit
instance StripeHasParam GetSubscriptions (StartingAfter SubscriptionId)

------------------------------------------------------------------------------
-- | Retrieve a customer's `Subscription`s
getSubscriptionsByCustomerId
    :: CustomerId
    -> StripeRequest GetSubscriptionsByCustomerId
getSubscriptionsByCustomerId
    customerid = request
  where request = mkStripeRequest GET url params
        url     = "customers" </> getCustomerId customerid </> "subscriptions"
        params  = []

data GetSubscriptionsByCustomerId
type instance StripeReturn GetSubscriptionsByCustomerId = StripeList Subscription
instance StripeHasParam GetSubscriptionsByCustomerId ExpandParams
instance StripeHasParam GetSubscriptionsByCustomerId (EndingBefore SubscriptionId)
instance StripeHasParam GetSubscriptionsByCustomerId Limit
instance StripeHasParam GetSubscriptionsByCustomerId (StartingAfter SubscriptionId)
