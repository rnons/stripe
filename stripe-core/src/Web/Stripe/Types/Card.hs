{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Web.Stripe.Types.Card where

import           Control.Monad                  (mzero)
import           Data.Aeson                     (FromJSON (parseJSON),
                                                 Value (String))
import           Data.Data                      (Data, Typeable)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Util                (getParams, toBytestring,
                                                 toText)


------------------------------------------------------------------------------
-- | City address for a `Card`
newtype AddressCity    = AddressCity Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressCity where
  toStripeParam (AddressCity txt) =
    (("address_city", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Country address for a `Card`
newtype AddressCountry = AddressCountry Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressCountry where
  toStripeParam (AddressCountry txt) =
    (("address_country", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Address Line One for a `Card`
newtype AddressLine1   = AddressLine1 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressLine1 where
  toStripeParam (AddressLine1 txt) =
    (("address_line1", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Address Line Two for a `Card`
newtype AddressLine2   = AddressLine2 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressLine2 where
  toStripeParam (AddressLine2 txt) =
    (("address_line2", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Address State for a `Card`
newtype AddressState   = AddressState Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressState where
  toStripeParam (AddressState txt) =
    (("address_state", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Address Zip Code for a `Card`
newtype AddressZip     = AddressZip Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam AddressZip where
  toStripeParam (AddressZip txt) =
    (("address_zip", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | Number associated with a `Card`
newtype CardNumber     = CardNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam CardNumber where
  toStripeParam (CardNumber num) =
    (("number", Text.encodeUtf8 num) :)

------------------------------------------------------------------------------
-- | a cardholder's full name
newtype Name  = Name { getName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Name where
  parseJSON v = Name <$> parseJSON v

instance ToStripeParam Name where
  toStripeParam (Name txt) =
    (("name", Text.encodeUtf8 txt) :)

------------------------------------------------------------------------------
-- | `NewCard` contains the data needed to create a new `Card`
data NewCard = NewCard
    { newCardCardNumber     :: CardNumber
    , newCardExpMonth       :: ExpMonth
    , newCardExpYear        :: ExpYear
    , newCardCVC            :: Maybe CVC
    , newCardName           :: Maybe Name
    , newCardAddressLine1   :: Maybe AddressLine1
    , newCardAddressLine2   :: Maybe AddressLine2
    , newCardAddressCity    :: Maybe AddressCity
    , newCardAddressZip     :: Maybe AddressZip
    , newCardAddressState   :: Maybe AddressState
    , newCardAddressCountry :: Maybe AddressCountry
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a `NewCard` with only the required fields
mkNewCard
    :: CardNumber
    -> ExpMonth
    -> ExpYear
    -> NewCard
mkNewCard
    cardNumber
    expMonth
    expYear
    = NewCard
    { newCardCardNumber     = cardNumber
    , newCardExpMonth       = expMonth
    , newCardExpYear        = expYear
    , newCardCVC            = Nothing
    , newCardName           = Nothing
    , newCardAddressLine1   = Nothing
    , newCardAddressLine2   = Nothing
    , newCardAddressCity    = Nothing
    , newCardAddressZip     = Nothing
    , newCardAddressState   = Nothing
    , newCardAddressCountry = Nothing
    }

instance ToStripeParam NewCard where
  toStripeParam NewCard{..} =
    ((getParams
        [ ("card[number]", Just $ (\(CardNumber x) -> x) newCardCardNumber)
        , ("card[exp_month]", Just $ (\(ExpMonth x) -> toText x) newCardExpMonth)
        , ("card[exp_year]", Just $ (\(ExpYear x) -> toText x) newCardExpYear)
        , ("card[cvc]", (\(CVC x) -> x) <$> newCardCVC)
        , ("card[name]", getName <$> newCardName)
        , ("card[address_city]", (\(AddressCity x) -> x) <$> newCardAddressCity)
        , ("card[address_country]", (\(AddressCountry x) -> x) <$> newCardAddressCountry)
        , ("card[address_line1]", (\(AddressLine1 x) -> x) <$> newCardAddressLine1 )
        , ("card[address_line2]", (\(AddressLine2 x) -> x) <$> newCardAddressLine2 )
        , ("card[address_state]", (\(AddressState x) -> x) <$> newCardAddressState )
        , ("card[address_zip]", (\(AddressZip x) -> x) <$> newCardAddressZip )
        ]) ++)

------------------------------------------------------------------------------
-- | Expiration Month for a `Card`
newtype ExpMonth       = ExpMonth Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ExpMonth where
  toStripeParam (ExpMonth m) =
    (("exp_month", toBytestring m) :)

------------------------------------------------------------------------------
-- | Expiration Year for a `Card`
newtype ExpYear        = ExpYear Int deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam ExpYear where
  toStripeParam (ExpYear y) =
    (("exp_year", toBytestring y) :)

------------------------------------------------------------------------------
-- | CVC for a `Card`
newtype CVC            = CVC Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam CVC where
  toStripeParam (CVC cvc) =
    (("cvc", Text.encodeUtf8 cvc) :)

------------------------------------------------------------------------------
-- | `IsVerified` `Recipients`
newtype IsVerified = IsVerified { getVerified :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam IsVerified where
  toStripeParam (IsVerified b) =
    (("verified", if b then "true" else "false") :)

------------------------------------------------------------------------------
-- | Credit / Debit Card Brand
data Brand = Visa
           | AMEX
           | MasterCard
           | Discover
           | JCB
           | DinersClub
           | Unknown
             deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Brand`
instance FromJSON Brand where
   parseJSON (String "American Express") = pure AMEX
   parseJSON (String "MasterCard")       = pure MasterCard
   parseJSON (String "Discover")         = pure Discover
   parseJSON (String "JCB")              = pure JCB
   parseJSON (String "Visa")             = pure Visa
   parseJSON (String "DinersClub")       = pure DinersClub
   parseJSON _                           = mzero

------------------------------------------------------------------------------
-- | CardId for a `Customer`
newtype CardId = CardId Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToStripeParam CardId where
  toStripeParam (CardId cid) =
    (("card", Text.encodeUtf8 cid) :)

------------------------------------------------------------------------------
-- | JSON Instance for `CardId`
instance FromJSON CardId where
   parseJSON (String x) = pure $ CardId x
   parseJSON _          = mzero

------------------------------------------------------------------------------
-- | set the `DefaultCard`
data DefaultCard = DefaultCard { getDefaultCard :: CardId }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam DefaultCard where
  toStripeParam (DefaultCard (CardId cid)) =
    (("default_card", Text.encodeUtf8 cid) :)
