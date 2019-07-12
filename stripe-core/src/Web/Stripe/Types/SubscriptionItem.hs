{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.SubscriptionItem where

import           Data.Aeson                     (FromJSON (parseJSON),
                                                 defaultOptions,
                                                 genericParseJSON,
                                                 unwrapUnaryRecords)
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           GHC.Generics                   (Generic)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

-- | `SubscriptionItemId` for a `SubscriptionItem`
newtype SubscriptionItemId = SubscriptionItemId { getSubscriptionItemId :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance FromJSON SubscriptionItemId where
    parseJSON = genericParseJSON defaultOptions
        { unwrapUnaryRecords = True }

instance ToStripeParam SubscriptionItemId where
  toStripeParam (SubscriptionItemId sid) =
    (("subscription_item", Text.encodeUtf8 sid) :)
