{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.TaxRate where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON (parseJSON), Value (String))
import           Data.Data     (Data, Typeable)
import           Data.Text     (Text)

------------------------------------------------------------------------------
-- | `TaxRateId` for a `TaxRate`
newtype TaxRateId = TaxRateId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TaxRateId where
    parseJSON (String x) = pure $ TaxRateId x
    parseJSON _          = mzero
