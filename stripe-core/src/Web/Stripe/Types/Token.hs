{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.Stripe.Types.Token where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | `TokenId` of a `Token`
newtype TokenId =
    TokenId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam TokenId where
  toStripeParam (TokenId tid) =
    (("source", Text.encodeUtf8 tid) :)

------------------------------------------------------------------------------
-- | Type of `Token`
data TokenType = TokenCard
               | TokenBankAccount
                 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TokenType`
instance FromJSON TokenType where
   parseJSON (String "bank_account") = pure TokenBankAccount
   parseJSON (String "card")         = pure TokenCard
   parseJSON _                       = mzero
