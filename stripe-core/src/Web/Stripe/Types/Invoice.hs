{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Invoice where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (Bool, String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `InvoiceId` for a `Invoice`
newtype InvoiceId =
    InvoiceId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceId`
instance FromJSON InvoiceId where
   parseJSON (String x) = pure $ InvoiceId x
   parseJSON _          = mzero


------------------------------------------------------------------------------
-- | `AutoAdvance` - perform automatic collection or not
newtype AutoAdvance = AutoAdvance { getAutoAdvance :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON AutoAdvance where
  parseJSON (Bool v) = pure $ AutoAdvance v
  parseJSON _        = mzero

instance ToStripeParam AutoAdvance where
  toStripeParam (AutoAdvance b) =
    (("auto_advance", if b then "true" else "false") :)

------------------------------------------------------------------------------
-- | `Closed` - invoice closed or not
newtype Closed =
  Closed { getClosed :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Closed where
  toStripeParam (Closed b) =
    (("closed", if b then "true" else "false") :)

------------------------------------------------------------------------------
-- | `Forgiven` - invoice forgiven or not
newtype Forgiven =
  Forgiven { getForgiven :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam Forgiven where
  toStripeParam (Forgiven b) =
    (("forgiven", if b then "true" else "false") :)

instance ToStripeParam InvoiceId where
  toStripeParam (InvoiceId txt) =
    (("invoice", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | `InvoiceLineItemId` for an `InvoiceLineItem`
newtype InvoiceLineItemId =
    InvoiceLineItemId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam InvoiceLineItemId where
  toStripeParam (InvoiceLineItemId txt) =
    (("line_item", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Type of `InvoiceItem`
data InvoiceLineItemType
    = InvoiceItemType |
     SubscriptionItemType
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItemType`
instance FromJSON InvoiceLineItemType where
   parseJSON (String "invoiceitem")  = pure InvoiceItemType
   parseJSON (String "subscription") = pure SubscriptionItemType
   parseJSON _                       = mzero
