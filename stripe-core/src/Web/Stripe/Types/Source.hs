{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Source where

import           Data.Data                      (Data, Typeable)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `Source` used for filtering `Balance` transactions. It should contain
-- an object Id such as a `ChargeId`
newtype Source a = Source { getSource :: a }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam a => ToStripeParam (Source a) where
  toStripeParam (Source param) =
    case toStripeParam param [] of
      [(_, p)] -> (("source", p) :)
      _        -> error "source applied to non-singleton"
