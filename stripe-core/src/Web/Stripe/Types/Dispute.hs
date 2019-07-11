{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Dispute where

import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `Evidence` associated with a `Dispute`
newtype Evidence = Evidence Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Evidence where
  toStripeParam (Evidence txt) =
    (("evidence", Text.encodeUtf8 txt) :)
