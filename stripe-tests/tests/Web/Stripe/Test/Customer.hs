{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Customer where
import           Data.Either
import           Data.Maybe
import           Test.Hspec
import           Web.Stripe.Customer
import           Web.Stripe.Test.Prelude

customerTests :: StripeSpec
customerTests stripe =
  describe "Customer tests" $ do
    it "Creates an empty customer" $ do
      result <- stripe $ do
        c@Customer{..} <- createCustomer
        _ <- deleteCustomer customerId
        return c
      result `shouldSatisfy` isRight
    it "Creates a customer with address" $ do
      result <- stripe $ do
        let
          addr = Address
            { country = Just "US"
            , state = Nothing
            , city = Nothing
            , line1 = "address line 1"
            , line2 = Nothing
            , postalCode = Nothing
            }
        c@Customer{..} <- createCustomer -&- addr
        _ <- deleteCustomer customerId
        return c
      result `shouldSatisfy` isRight
    it "Deletes a customer" $ do
      result <- stripe $ do
        c@Customer{..} <- createCustomer
        _ <- deleteCustomer customerId
        return c
      result `shouldSatisfy` isRight
    it "Gets a customer" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        customer <- getCustomer cid
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
    it "Gets a customer expandable" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        customer <- getCustomer cid -&- ExpandParams ["default_card"]
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
    it "Gets customers" $ do
      result <- stripe $ void $ getCustomers -&- Limit 100
      result `shouldSatisfy` isRight
    it "Gets customers expandable" $ do
      result <- stripe $ void $ getCustomers
                 -&- ExpandParams ["data.default_card"]
      result `shouldSatisfy` isRight
    xit "Updates a customer" $ do
      result <- stripe $ do
        Customer { customerId = cid } <- createCustomer
        customer <- updateCustomer cid
                      -&- bal
                      -&- desc
                      -&- email
                      -&- meta
        _ <- deleteCustomer cid
        return customer
      result `shouldSatisfy` isRight
      let Right Customer{..} = result
      (AccountBalance customerBalance) `shouldBe` bal
      customerDescription `shouldBe` (Just desc)
      customerEmail `shouldBe` (Just email)
      customerMetadata `shouldBe` meta
  where
    bal   = AccountBalance 100
    desc  = Description "hey"
    email = Email "djohnson.m@gmail.com"
    meta  = Metadata [("hey","there")]
