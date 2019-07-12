{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Stripe.SubscriptionItem
    ( -- * API
      CreateSubscriptionItem
    , createSubscriptionItem
    , GetSubscriptionItem
    , getSubscriptionItem
    , UpdateSubscriptionItem
    , updateSubscriptionItem
    , DeleteSubscriptionItem
    , deleteSubscriptionItem
    , GetSubscriptionItems
    , getSubscriptionItems
      -- * Types
    , EndingBefore(..)
    , Limit(..)
    , Metadata(..)
    , PlanId(..)
    , Prorate(..)
    , Quantity(..)
    , StartingAfter(..)
    , SubscriptionId(..)
    , SubscriptionItem(..)
    , SubscriptionItemId(..)
    , TaxRates(..)
    ) where

import           Web.Stripe.StripeRequest          (Method (DELETE, POST),
                                                    StripeHasParam,
                                                    StripeRequest (..),
                                                    StripeReturn,
                                                    ToStripeParam (..),
                                                    mkStripeRequest)
import           Web.Stripe.Types                  (EndingBefore (..),
                                                    Limit (..), Metadata (..),
                                                    Quantity (..),
                                                    StartingAfter (..),
                                                    StripeList (..),
                                                    SubscriptionItem (..))
import           Web.Stripe.Types.Plan             (PlanId (..))
import           Web.Stripe.Types.Subscription     (Prorate (..),
                                                    SubscriptionId (..))
import           Web.Stripe.Types.SubscriptionItem (SubscriptionItemId (..))
import           Web.Stripe.Types.TaxRate          (TaxRates (..))
import           Web.Stripe.Util                   ((</>))

------------------------------------------------------------------------------
-- | Update a `SubscriptionItem` by `PlanId` and `SubscriptionItemId`
createSubscriptionItem
  :: PlanId
  -> SubscriptionId
  -> StripeRequest CreateSubscriptionItem
createSubscriptionItem planId subscriptionId =
    mkStripeRequest POST url params
  where
    url = "subscription_items"
    params =
        toStripeParam planId $
        toStripeParam subscriptionId []

data CreateSubscriptionItem
type instance StripeReturn CreateSubscriptionItem = SubscriptionItem
-- instance StripeHasParam CreateSubscriptionItem BillingThresholds
instance StripeHasParam CreateSubscriptionItem Metadata
instance StripeHasParam CreateSubscriptionItem Prorate
-- instance StripeHasParam CreateSubscriptionItem ProrationDate
instance StripeHasParam CreateSubscriptionItem Quantity
instance StripeHasParam CreateSubscriptionItem TaxRates

------------------------------------------------------------------------------
-- | Retrieve a `SubscriptionItem` by `SubscriptionItemId`
getSubscriptionItem
  :: SubscriptionItemId
  -> StripeRequest GetSubscriptionItem
getSubscriptionItem subscriptionItemId =
    mkStripeRequest POST url params
  where
    url = "subscription_items" </> getSubscriptionItemId subscriptionItemId
    params = []

data GetSubscriptionItem
type instance StripeReturn GetSubscriptionItem = SubscriptionItem

------------------------------------------------------------------------------
-- | Update a `SubscriptionItem` by `SubscriptionItemId`
updateSubscriptionItem
  :: SubscriptionItemId
  -> StripeRequest UpdateSubscriptionItem
updateSubscriptionItem subscriptionItemId =
    mkStripeRequest POST url params
  where
    url = "subscription_items" </> getSubscriptionItemId subscriptionItemId
    params = []

data UpdateSubscriptionItem
type instance StripeReturn UpdateSubscriptionItem = SubscriptionItem
-- instance StripeHasParam UpdateSubscriptionItem BillingThresholds
instance StripeHasParam UpdateSubscriptionItem Metadata
instance StripeHasParam UpdateSubscriptionItem PlanId
instance StripeHasParam UpdateSubscriptionItem Prorate
-- instance StripeHasParam UpdateSubscriptionItem ProrationDate
instance StripeHasParam UpdateSubscriptionItem Quantity
instance StripeHasParam UpdateSubscriptionItem TaxRates

------------------------------------------------------------------------------
-- | Delete a `SubscriptionItem` by `SubscriptionItemId`
deleteSubscriptionItem
  :: SubscriptionItemId
  -> StripeRequest DeleteSubscriptionItem
deleteSubscriptionItem subscriptionItemId =
    mkStripeRequest DELETE url params
  where
    url = "subscription_items" </> getSubscriptionItemId subscriptionItemId
    params = []

data DeleteSubscriptionItem
type instance StripeReturn DeleteSubscriptionItem = SubscriptionItem
-- instance StripeHasParam DeleteSubscriptionItem ClearUsage
instance StripeHasParam DeleteSubscriptionItem Prorate
-- instance StripeHasParam DeleteSubscriptionItem ProrationDate

------------------------------------------------------------------------------
-- | Retrieve a list of `SubscriptionItem` by `SubscriptionId`
getSubscriptionItems
  :: SubscriptionId
  -> StripeRequest GetSubscriptionItems
getSubscriptionItems subscriptionId =
    mkStripeRequest POST url params
  where
    url = "subscription_items"
    params = toStripeParam subscriptionId []

data GetSubscriptionItems
type instance StripeReturn GetSubscriptionItems = StripeList SubscriptionItem
instance StripeHasParam GetSubscriptionItems (EndingBefore SubscriptionItemId)
instance StripeHasParam GetSubscriptionItems Limit
instance StripeHasParam GetSubscriptionItems (StartingAfter SubscriptionItemId)
