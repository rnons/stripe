{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.StatementDescriptor where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `StatementDescription` to be added to a `Charge`
newtype StatementDescription =
  StatementDescription Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON StatementDescription where
  parseJSON v = StatementDescription <$> parseJSON v

instance ToStripeParam StatementDescription where
  toStripeParam (StatementDescription txt) =
    (("statement_description", Text.encodeUtf8 txt) :)
