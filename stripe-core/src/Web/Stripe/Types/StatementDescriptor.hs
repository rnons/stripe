{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.StatementDescriptor where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `StatementDescriptor` to be added to a `Charge`
newtype StatementDescriptor =
  StatementDescriptor Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON StatementDescriptor where
  parseJSON v = StatementDescriptor <$> parseJSON v

instance ToStripeParam StatementDescriptor where
  toStripeParam (StatementDescriptor txt) =
    (("statement_descriptor", Text.encodeUtf8 txt) :)
