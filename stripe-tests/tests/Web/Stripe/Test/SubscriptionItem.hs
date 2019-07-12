{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.SubscriptionItem where

import           Data.Either

import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util

import           Web.Stripe.SubscriptionItem

subscriptionItemTests :: StripeSpec
subscriptionItemTests stripe = do
  describe "SubscriptionItem tests" $ do
    it "Succesfully creates a SubscriptionItem" $ do
      planId <- makePlanId
      subscriptionId <- makeSubscriptionId
      result <- stripe $ do
        item <- createSubscriptionItem planId subscriptionId
        return item
      result `shouldSatisfy` isRight
