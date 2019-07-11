{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.Stripe.Types.Recipient where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Text.Read                      (lexP, pfail)
import qualified Text.Read                      as R
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (toBytestring)

------------------------------------------------------------------------------
-- | `RecipientId` for a `Recipient`
newtype RecipientId =
      RecipientId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientId`
instance FromJSON RecipientId where
   parseJSON (String x) = pure $ RecipientId x
   parseJSON _          = mzero

instance ToStripeParam RecipientId where
  toStripeParam (RecipientId rid) =
    (("recipient", Text.encodeUtf8 rid) :)

------------------------------------------------------------------------------
-- | Type of `Recipient`
data RecipientType =
    Individual
  | Corporation deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Show` instance for `RecipientType`
instance Show RecipientType where
    show Individual  = "individual"
    show Corporation = "corporation"

------------------------------------------------------------------------------
-- | `Read` instance for `RecipientType`
instance Read RecipientType where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "individual"  -> return Individual
         "corporation" -> return Corporation
         _             -> pfail

------------------------------------------------------------------------------
-- | JSON Instance for `RecipientType`
instance FromJSON RecipientType where
   parseJSON (String "individual")  = pure Individual
   parseJSON (String "corporation") = pure Corporation
   parseJSON _                      = mzero

instance ToStripeParam RecipientType where
  toStripeParam recipientType =
    (("type", toBytestring recipientType) :)
