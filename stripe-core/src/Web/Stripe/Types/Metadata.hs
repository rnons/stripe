{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Metadata where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import qualified Data.HashMap.Strict            as H
import           Data.Text                      (Text)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toMetadata)

------------------------------------------------------------------------------
-- | Type of Metadata for use on `Stripe` objects
newtype Metadata = Metadata [ (Text,Text) ]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Metadata where
  parseJSON j = (Metadata . H.toList) <$> (parseJSON j)

instance ToStripeParam Metadata where
  toStripeParam (Metadata kvs) =
    (toMetadata kvs ++)
