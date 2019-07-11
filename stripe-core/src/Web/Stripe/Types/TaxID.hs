{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.TaxID where

import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `TaxID`
newtype TaxID  = TaxID { getTaxID :: Text }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TaxID where
  toStripeParam (TaxID tid) =
    (("tax_id", Text.encodeUtf8 tid) :)
