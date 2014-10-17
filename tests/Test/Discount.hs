{-# LANGUAGE OverloadedStrings #-}
module Test.Discount where

import           Data.Either
import           Test.Config        (getConfig)
import           Test.Hspec
import           Web.Stripe
import           Web.Stripe.Account

discountTests :: Spec
discountTests = do
  describe "Dispute tests" $ do
    it "Succesfully retrieves account information" $ do
      config <- getConfig
      result <- stripe config getAccountDetails
      result `shouldSatisfy` isRight

