module Web.Stripe.StripeRequest.Class
  ( ToStripeParam(..)
  ) where

import           Data.ByteString (ByteString)

-- | convert a parameter to a key/value
class ToStripeParam param where
  toStripeParam :: param -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
