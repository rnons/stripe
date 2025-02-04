{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Recipient
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#recipients >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $
--       createRecipient (Name "simon marlow")
--                       Individual
--   case result of
--     Right recipient  -> print recipient
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Recipient
    ( -- * API
      CreateRecipient
    , createRecipient
    , GetRecipient
    , getRecipient
    , UpdateRecipient
    , updateRecipient
    , DeleteRecipient
    , deleteRecipient
    , GetRecipients
    , getRecipients
      -- * Types
    , AccountNumber  (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressState   (..)
    , AddressZip     (..)
    , BankAccount    (..)
    , BankAccountId  (..)
    , BankAccountStatus (..)
    , CardNumber     (..)
    , Country        (..)
    , CVC            (..)
    , DefaultCard    (..)
    , Description    (..)
    , ExpandParams   (..)
    , ExpMonth       (..)
    , ExpYear        (..)
    , Email          (..)
    , IsVerified     (..)
    , Limit          (..)
    , Metadata       (..)
    , Name           (..)
    , NewBankAccount (..)
    , NewCard        (..)
    , Recipient      (..)
    , RecipientId    (..)
    , RecipientType  (..)
    , RoutingNumber  (..)
    , StripeDeleteResult (..)
    , TaxID          (..)
    , TokenId        (..)
    ) where

import           Web.Stripe.StripeRequest (Method (DELETE, GET, POST),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam (..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (AccountNumber (..),
                                           AccountNumber (..), AddressCity (..),
                                           AddressCountry (..),
                                           AddressLine1 (..), AddressLine2 (..),
                                           AddressState (..), AddressZip (..),
                                           BankAccount (..), BankAccountId (..),
                                           BankAccountStatus (..), CVC (..),
                                           CardId (..), CardNumber,
                                           CardNumber (..), Country (..),
                                           Country (..), DefaultCard (..),
                                           Description (..), Email (..),
                                           EndingBefore (..), ExpMonth (..),
                                           ExpYear (..), ExpYear (..),
                                           ExpandParams (..), IsVerified (..),
                                           Limit (..), Metadata (..),
                                           NewBankAccount (..), NewCard (..),
                                           Recipient (..), RecipientId (..),
                                           RecipientType (..),
                                           RoutingNumber (..),
                                           RoutingNumber (..),
                                           StartingAfter (..),
                                           StripeDeleteResult (..),
                                           StripeList (..), TokenId (..),
                                           TokenId (..))
import           Web.Stripe.Types.Name    (Name (..))
import           Web.Stripe.Types.TaxID   (TaxID (..))
import           Web.Stripe.Types.Util    (getRecipientId)
import           Web.Stripe.Util          ((</>))

------------------------------------------------------------------------------
-- | Base Request for issues create `Recipient` requests
createRecipient
    :: Name          -- ^ `Recipient` `Name`
    -> RecipientType -- ^ `Individual` or `Corporation`
    -> StripeRequest CreateRecipient
createRecipient
    name
    recipienttype
                = request
  where request = mkStripeRequest POST url params
        url     = "recipients"
        params  = toStripeParam name $
                  toStripeParam recipienttype $
                  []

data CreateRecipient
type instance StripeReturn CreateRecipient = Recipient
instance StripeHasParam CreateRecipient TaxID
instance StripeHasParam CreateRecipient NewBankAccount
instance StripeHasParam CreateRecipient TokenId
instance StripeHasParam CreateRecipient NewCard
instance StripeHasParam CreateRecipient CardId
instance StripeHasParam CreateRecipient Email
instance StripeHasParam CreateRecipient Description
instance StripeHasParam CreateRecipient Metadata

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
getRecipient
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> StripeRequest GetRecipient
getRecipient
  recipientid   = request
  where request = mkStripeRequest GET url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data GetRecipient
type instance StripeReturn GetRecipient = Recipient
instance StripeHasParam GetRecipient ExpandParams

------------------------------------------------------------------------------
-- | Update `Recipient`
updateRecipient
    :: RecipientId -- ^ `RecipientId` of `Recipient` to update
    -> StripeRequest UpdateRecipient
updateRecipient
  recipientid   = request
  where request = mkStripeRequest POST url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data UpdateRecipient
type instance StripeReturn UpdateRecipient = Recipient
instance StripeHasParam UpdateRecipient Name
instance StripeHasParam UpdateRecipient TaxID
instance StripeHasParam UpdateRecipient NewBankAccount
instance StripeHasParam UpdateRecipient TokenId
instance StripeHasParam UpdateRecipient NewCard
instance StripeHasParam UpdateRecipient DefaultCard
instance StripeHasParam UpdateRecipient CardId
instance StripeHasParam UpdateRecipient Email
instance StripeHasParam UpdateRecipient Description
instance StripeHasParam UpdateRecipient Metadata

------------------------------------------------------------------------------
-- | Delete a `Recipient`
deleteRecipient
    :: RecipientId   -- ^ `RecipiendId` of `Recipient` to delete
    -> StripeRequest DeleteRecipient
deleteRecipient
   recipientid = request
  where request = mkStripeRequest DELETE url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data DeleteRecipient
type instance StripeReturn DeleteRecipient = StripeDeleteResult

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's
getRecipients
    :: StripeRequest GetRecipients
getRecipients
  = request
  where request = mkStripeRequest GET url params
        url     = "recipients"
        params  = []

data GetRecipients
type instance StripeReturn GetRecipients = StripeList Recipient
instance StripeHasParam GetRecipients ExpandParams
instance StripeHasParam GetRecipients (EndingBefore RecipientId)
instance StripeHasParam GetRecipients Limit
instance StripeHasParam GetRecipients (StartingAfter RecipientId)
instance StripeHasParam GetRecipients IsVerified
