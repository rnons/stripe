{-# LANGUAGE DeriveDataTypeable #-}
module Web.Stripe.Types.Country where

import           Data.Data (Data, Typeable)
import           Data.Text (Text)

------------------------------------------------------------------------------
-- | Country
newtype Country = Country { getCountry :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable)
