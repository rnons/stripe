{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.StripeList where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (Object), (.:), (.:?))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | Generic handling of Stripe JSON arrays
data StripeList a = StripeList {
      list       :: [a]
    , stripeUrl  :: Text
    , object     :: Text
    , totalCount :: Maybe Int
    , hasMore    :: Bool
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeList`
instance FromJSON a => FromJSON (StripeList a) where
    parseJSON (Object o) =
        StripeList <$> o .:  "data"
                   <*> o .:  "url"
                   <*> o .:  "object"
                   <*> o .:? "total_count"
                   <*> o .:  "has_more"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype Limit = Limit Int
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Limit where
  toStripeParam (Limit i) =
    (("limit", toBytestring i) :)

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype StartingAfter a = StartingAfter a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

instance (ToStripeParam param) => ToStripeParam (StartingAfter param) where
  toStripeParam (StartingAfter param) =
    case toStripeParam param [] of
      [(_, p)] -> (("starting_after", p) :)
      _        -> error "StartingAfter applied to non-singleton"

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype EndingBefore a = EndingBefore a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

instance (ToStripeParam param) => ToStripeParam (EndingBefore param) where
  toStripeParam (EndingBefore param) =
    case toStripeParam param [] of
      [(_, p)] -> (("ending_before", p) :)
      _        -> error "EndingBefore applied to non-singleton"
