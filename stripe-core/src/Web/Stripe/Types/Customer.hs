{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Customer where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | AccountBalance for a `Customer`
newtype AccountBalance = AccountBalance Int
  deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToStripeParam AccountBalance where
  toStripeParam (AccountBalance i) =
    (("account_balance", toBytestring i) :)

------------------------------------------------------------------------------
-- | `CustomerId` for a `Customer`
newtype CustomerId = CustomerId { getCustomerId :: Text }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CustomerId`
instance FromJSON CustomerId where
    parseJSON (String x) = pure (CustomerId x)
    parseJSON _          = mzero

instance ToStripeParam CustomerId where
  toStripeParam (CustomerId cid) =
    (("customer", Text.encodeUtf8 cid) :)
