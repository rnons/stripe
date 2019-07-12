{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Web.Stripe.Types.SubscriptionItem where

import           Data.Aeson   (FromJSON (parseJSON), defaultOptions,
                               genericParseJSON, unwrapUnaryRecords)
import           Data.Data    (Data, Typeable)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | `SubscriptionItemId` for a `SubscriptionItem`
newtype SubscriptionItemId = SubscriptionItemId { getSubscriptionItemId :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance FromJSON SubscriptionItemId where
    parseJSON = genericParseJSON defaultOptions
        { unwrapUnaryRecords = True }
