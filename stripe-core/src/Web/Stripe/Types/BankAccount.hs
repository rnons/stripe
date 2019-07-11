{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Web.Stripe.Types.BankAccount where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Types.Country       (Country (..))
import           Web.Stripe.Util                (getParams)

------------------------------------------------------------------------------
-- | `BankAccountId` for `BankAccount`
newtype BankAccountId = BankAccountId Text
                        deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam BankAccountId where
  toStripeParam (BankAccountId bid) =
    (("bank_account", Text.encodeUtf8 bid) :)

------------------------------------------------------------------------------
-- | `BankAccountStatus` Object
data BankAccountStatus =
  New | Validated | Verified | Errored
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountStatus` JSON instance
instance FromJSON BankAccountStatus where
   parseJSON (String "new")       = pure $ New
   parseJSON (String "validated") = pure Validated
   parseJSON (String "verified")  = pure Verified
   parseJSON (String "errored")   = pure Errored
   parseJSON _                    = mzero

------------------------------------------------------------------------------
-- | Routing Number for Bank Account
newtype RoutingNumber =
  RoutingNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Account Number of a Bank Account
newtype AccountNumber =
  AccountNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a new `BankAccount`
data NewBankAccount = NewBankAccount
    { newBankAccountCountry       :: Country
    , newBankAccountRoutingNumber :: RoutingNumber
    , newBankAccountAccountNumber :: AccountNumber
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam NewBankAccount where
  toStripeParam NewBankAccount{..} =
    ((getParams
        [ ("bank_account[country]", Just $ (\(Country x) -> x) newBankAccountCountry)
        , ("bank_account[routing_number]", Just $ (\(RoutingNumber x) -> x) newBankAccountRoutingNumber)
        , ("bank_account[account_number]", Just $ (\(AccountNumber x) -> x) newBankAccountAccountNumber)
        ]) ++)
