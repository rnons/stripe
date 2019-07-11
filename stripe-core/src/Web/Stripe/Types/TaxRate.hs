{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.TaxRate where

import           Data.Data (Data, Typeable)
import           Data.Text (Text)

------------------------------------------------------------------------------
-- | `TaxRateId` for a `TaxRate`
newtype TaxRateId = TaxRateId Text deriving (Read, Show, Eq, Ord, Data, Typeable)
