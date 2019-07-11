{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.MetaData where

import           Data.Aeson                     (FromJSON (parseJSON))
import           Data.Data                      (Data, Typeable)
import qualified Data.HashMap.Strict            as H
import           Data.Text                      (Text)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toMetaData)

------------------------------------------------------------------------------
-- | Type of MetaData for use on `Stripe` objects
newtype MetaData = MetaData [ (Text,Text) ]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON MetaData where
  parseJSON j = (MetaData . H.toList) <$> (parseJSON j)

instance ToStripeParam MetaData where
  toStripeParam (MetaData kvs) =
    (toMetaData kvs ++)
