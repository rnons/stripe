{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Description where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | Generic Description for use in constructing API Calls
newtype Description = Description Text
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Description where
  parseJSON v = Description <$> parseJSON v

instance ToStripeParam Description where
  toStripeParam (Description txt) =
    (("description", Text.encodeUtf8 txt) :)
