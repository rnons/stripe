{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.InvoiceItem where

import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `InvoiceItemId` for `InvoiceItem`
newtype InvoiceItemId
    = InvoiceItemId Text
      deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam InvoiceItemId where
  toStripeParam (InvoiceItemId txt) =
    (("id", Text.encodeUtf8 txt) :)
