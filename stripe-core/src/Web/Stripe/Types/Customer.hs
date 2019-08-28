{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Web.Stripe.Types.Customer where

import           Control.Monad                  (join)
import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String), camelTo2,
                                                 defaultOptions,
                                                 fieldLabelModifier,
                                                 genericParseJSON)
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           GHC.Generics                   (Generic)
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

data Address = Address
  { country    :: Maybe Text
  , state      :: Maybe Text
  , city       :: Maybe Text
  , line1      :: Text
  , line2      :: Maybe Text
  , postalCode :: Maybe Text
  } deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToStripeParam Address where
  toStripeParam (Address {..}) = (join
    [ pure ("address[line1]", Text.encodeUtf8 line1)
    , maybe [] (\v -> pure ("address[country]", Text.encodeUtf8 v)) country
    , maybe [] (\v -> pure ("address[state]", Text.encodeUtf8 v)) state
    , maybe [] (\v -> pure ("address[city]", Text.encodeUtf8 v)) city
    , maybe [] (\v -> pure ("address[line2]", Text.encodeUtf8 v)) line2
    , maybe [] (\v -> pure ("address[postal_code]", Text.encodeUtf8 v)) postalCode
    ] <>)

instance FromJSON Address where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' }
