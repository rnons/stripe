{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Name where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `Name` for a Card, Customer
newtype Name  = Name { getName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Name where
  parseJSON v = Name <$> parseJSON v

instance ToStripeParam Name where
  toStripeParam (Name txt) =
    (("name", Text.encodeUtf8 txt) :)
