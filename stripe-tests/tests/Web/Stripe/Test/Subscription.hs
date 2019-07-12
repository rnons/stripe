{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Subscription where

import           Data.Either
import           Data.Maybe

import           Test.Hspec
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Test.Util

import           Web.Stripe.Coupon
import           Web.Stripe.Customer
import           Web.Stripe.Plan
import           Web.Stripe.StripeRequest
import           Web.Stripe.Subscription
import           Web.Stripe.Types.TaxRate (TaxRateId (..))

createSubscription'
    :: CustomerId
    -> PlanId
    -> StripeRequest CreateSubscription
createSubscription' cid pid = createSubscription cid
    [CreateSubscriptionSubscriptionItem pid Nothing Nothing Nothing]

subscriptionTests :: StripeSpec
subscriptionTests stripe = do
  describe "Subscription tests" $ do
    it "Succesfully creates a Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        sub <- createSubscription cid
            [ CreateSubscriptionSubscriptionItem planid Nothing Nothing $
                Just [TaxRateId "txr_123"]
            ]
            -&- DefaultTaxRates [TaxRateId "txr_def"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        Subscription { id = sid } <- createSubscription' cid planid
        sub <- getSubscription cid sid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Subscription expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        Subscription { id = sid } <- createSubscription' cid planid
        sub <- getSubscription cid sid -&- ExpandParams ["customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        void $ createSubscription' cid planid
        sub <- getSubscriptionsByCustomerId cid -&- ExpandParams ["data.customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a Customer's Subscriptions" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        void $ createSubscription' cid planid
        sub <- getSubscriptionsByCustomerId cid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves all Subscriptions expanded" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        void $ createSubscription' cid planid
        sub <- getSubscriptions -&- ExpandParams ["data.customer"]
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    it "Succesfully retrieves all Subscriptions" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        void $ createSubscription' cid planid
        sub <- getSubscriptions
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
    xit "Succesfully updates a Customer's Subscriptions" $ do
      planid <- makePlanId
      secondPlanid <- makePlanId
      couponid <- makeCouponId
      result <- stripe $ do
        Coupon { } <-
          createCoupon
             (Just couponid)
             Once
             -&- (AmountOff 1)
             -&- USD
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        Subscription { id = sid } <- createSubscription' cid planid
        sub <- updateSubscription cid sid
                -&- couponid
                -&- Metadata [("hi","there")]
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      metadata `shouldBe` (Metadata [("hi", "there")])
      discount `shouldSatisfy` isJust
    xit "Succesfully cancels a Customer's Subscription" $ do
      planid <- makePlanId
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        void $ createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
        Subscription { id = sid } <- createSubscription' cid planid
        sub <- cancelSubscription cid sid
        void $ deletePlan planid
        void $ deleteCustomer cid
        return sub
      result `shouldSatisfy` isRight
      let Right Subscription {..} = result
      status `shouldBe` Canceled
