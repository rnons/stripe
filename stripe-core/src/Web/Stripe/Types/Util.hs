{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Types.Util
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Types.Util
       ( -- * Getters
         getTransactionId
       , getCustomerId
       , getCardId
       , getRecipientCardId
       , getRecipientId
       , getAccountId
       , getChargeId
       , getInvoiceId
       , getInvoiceItemId
       ) where

import           Data.Text                    (Text)
import           Web.Stripe.Types
import           Web.Stripe.Types.Card        (CardId (..))
import           Web.Stripe.Types.Invoice     (InvoiceId (..))
import           Web.Stripe.Types.InvoiceItem (InvoiceItemId (..))
import           Web.Stripe.Types.Transaction (TransactionId (..))

------------------------------------------------------------------------------
-- | Helper for retrieving `TransactionId`
getTransactionId :: TransactionId -> Text
getTransactionId (TransactionId x) = x

------------------------------------------------------------------------------
-- | Helper for retrieving `RecipientCardId`
getRecipientCardId :: RecipientCardId -> Text
getRecipientCardId (RecipientCardId x)  = x

------------------------------------------------------------------------------
-- | Helper for retrieiving `RecipientId`
getRecipientId :: RecipientId -> Text
getRecipientId (RecipientId x) = x

------------------------------------------------------------------------------
-- | Helper for retrieving `AccountId`
getAccountId :: AccountId -> Text
getAccountId (AccountId x) = x

------------------------------------------------------------------------------
-- | Helper for retrieving `CardId`
getChargeId :: ChargeId -> Text
getChargeId (ChargeId x) = x

------------------------------------------------------------------------------
-- | Helper for retrieving `InvoiceId`
getInvoiceId :: InvoiceId -> Text
getInvoiceId (InvoiceId x) = x

------------------------------------------------------------------------------
-- | Helper for retrieving `InvoiceItemId`
getInvoiceItemId :: InvoiceItemId -> Text
getInvoiceItemId (InvoiceItemId x) = x
