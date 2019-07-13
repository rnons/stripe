{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Invoice
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#invoices >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Customer
-- import Web.Stripe.Invoice
-- import Web.Stripe.InvoiceItem
-- import Web.Stripe.Plan
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (SecretKey "secret_key")
--   result <- stripe config createCustomer
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Customer { customerId = cid })) ->
--       do result <- stripe config $
--            createPlan (PlanId "planid") (Amount 20) USD Day (PlanName "testplan")
--          case result of
--            (Left stripeError) -> print stripeError
--            (Right (Plan {})) ->
--              do result <- stripe config $
--                   createInvoiceItem cid (Amount 100) USD
--                 case result of
--                   (Left stripeError)  -> print stripeError
--                   (Right invoiceItem) ->
--                      do result <- stripe config $ createInvoice cid
--                         case result of
--                           (Left  stripeError) -> print stripeError
--                           (Right invoice)     -> print invoice
-- @
module Web.Stripe.Invoice
    ( -- * API
      CreateInvoice
    , createInvoice
    , GetInvoice
    , getInvoice
    , GetInvoiceLineItems
    , getInvoiceLineItems
    , GetUpcomingInvoice
    , getUpcomingInvoice
    , UpdateInvoice
    , updateInvoice
    , PayInvoice
    , payInvoice
    , GetInvoices
    , getInvoices
       -- * Types
    , ApplicationFeeId    (..)
    , Closed              (..)
    , CollectionMethod(..)
    , CustomerId          (..)
    , DefaultTaxRates(..)
    , Description         (..)
    , Discount            (..)
    , EndingBefore        (..)
    , ExpandParams        (..)
    , Forgiven            (..)
    , Invoice             (..)
    , InvoiceId           (..)
    , InvoiceLineItem     (..)
    , InvoiceLineItemId   (..)
    , InvoiceLineItemType (..)
    , Limit               (..)
    , Metadata            (..)
    , Period              (..)
    , StatementDescriptor(..)
    , StartingAfter       (..)
    , StripeList          (..)
    , SubscriptionId      (..)
    ) where

import           Web.Stripe.StripeRequest          (Method (GET, POST),
                                                    StripeHasParam,
                                                    StripeRequest (..),
                                                    StripeReturn,
                                                    mkStripeRequest,
                                                    toStripeParam)
import           Web.Stripe.Types                  (ApplicationFeeId (..),
                                                    CustomerId (..),
                                                    Description (..),
                                                    Discount (..),
                                                    EndingBefore (..),
                                                    ExpandParams (..),
                                                    Invoice (..),
                                                    InvoiceLineItem (..),
                                                    Limit (..), Metadata (..),
                                                    Period (..),
                                                    StartingAfter (..),
                                                    StatementDescriptor (..),
                                                    StripeList (..),
                                                    SubscriptionId (..))
import           Web.Stripe.Types.CollectionMethod (CollectionMethod (..))
import           Web.Stripe.Types.Invoice          (Closed (..), Forgiven (..),
                                                    InvoiceId (..),
                                                    InvoiceLineItemId (..),
                                                    InvoiceLineItemType (..))
import           Web.Stripe.Types.TaxRate          (DefaultTaxRates (..))
import           Web.Stripe.Types.Util             (getInvoiceId)
import           Web.Stripe.Util                   ((</>))

------------------------------------------------------------------------------
-- | The `Invoice` to be created for a `Customer`
createInvoice
    :: CustomerId -- ^ `CustomerId` of `Customer` to `Invoice`
    -> StripeRequest CreateInvoice
createInvoice
    customerid
                = request
  where request = mkStripeRequest POST url params
        url     = "invoices"
        params  = toStripeParam customerid $
                  []

data CreateInvoice
type instance StripeReturn CreateInvoice = Invoice
-- instance StripeHasParam CreateInvoice ApplicationFeeAmount
-- instance StripeHasParam CreateInvoice AutoAdvance
instance StripeHasParam CreateInvoice CollectionMethod
-- instance StripeHasParam CreateInvoice CustomFields
-- instance StripeHasParam CreateInvoice DaysUntilDue
-- instance StripeHasParam CreateInvoice DefaultPaymentMethod
-- instance StripeHasParam CreateInvoice DefaultSource
instance StripeHasParam CreateInvoice DefaultTaxRates
instance StripeHasParam CreateInvoice Description
-- instance StripeHasParam CreateInvoice DueDate
-- instance StripeHasParam CreateInvoice Footer
instance StripeHasParam CreateInvoice Metadata
instance StripeHasParam CreateInvoice StatementDescriptor
instance StripeHasParam CreateInvoice SubscriptionId

------------------------------------------------------------------------------
-- | Retrieve an `Invoice` by `InvoiceId`
getInvoice
    :: InvoiceId -- ^ Get an `Invoice` by `InvoiceId`
    -> StripeRequest GetInvoice
getInvoice
    invoiceid = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = []

data GetInvoice
type instance StripeReturn GetInvoice = Invoice
instance StripeHasParam GetInvoice ExpandParams

------------------------------------------------------------------------------
-- | Retrieve a `StripeList` of `Invoice`s
getInvoices
    :: StripeRequest GetInvoices
getInvoices = request
  where request = mkStripeRequest GET url params
        url     = "invoices"
        params  = []

data GetInvoices
type instance StripeReturn GetInvoices = StripeList Invoice
instance StripeHasParam GetInvoices ExpandParams
instance StripeHasParam GetInvoices (EndingBefore InvoiceId)
instance StripeHasParam GetInvoices Limit
instance StripeHasParam GetInvoices CustomerId
instance StripeHasParam GetInvoices (StartingAfter InvoiceId)

------------------------------------------------------------------------------
-- | Retrieve an `InvoiceLineItem`s by `InvoiceId`
getInvoiceLineItems
    :: InvoiceId                       -- ^ Get an `Invoice` by `InvoiceId`
    -> StripeRequest GetInvoiceLineItems
getInvoiceLineItems
    invoiceid   = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> getInvoiceId invoiceid </> "lines"
        params  = []

data GetInvoiceLineItems
type instance StripeReturn GetInvoiceLineItems = StripeList InvoiceLineItem
instance StripeHasParam GetInvoiceLineItems CustomerId
instance StripeHasParam GetInvoiceLineItems (EndingBefore InvoiceLineItemId)
instance StripeHasParam GetInvoiceLineItems Limit
instance StripeHasParam GetInvoiceLineItems (StartingAfter InvoiceLineItemId)
instance StripeHasParam GetInvoiceLineItems SubscriptionId

------------------------------------------------------------------------------
-- | Retrieve an upcoming `Invoice` for a `Customer` by `CustomerId`
getUpcomingInvoice
    :: CustomerId -- ^ The `InvoiceId` of the `Invoice` to retrieve
    -> StripeRequest GetUpcomingInvoice
getUpcomingInvoice
    customerid = request
  where request = mkStripeRequest GET url params
        url     = "invoices" </> "upcoming"
        params  = toStripeParam customerid []

data GetUpcomingInvoice
type instance StripeReturn GetUpcomingInvoice = Invoice
instance StripeHasParam GetUpcomingInvoice SubscriptionId

------------------------------------------------------------------------------
-- | Update `Invoice` by `InvoiceId`
updateInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to update
    -> StripeRequest UpdateInvoice
updateInvoice
    invoiceid   = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid
        params  = []

data UpdateInvoice
type instance StripeReturn UpdateInvoice = Invoice
instance StripeHasParam UpdateInvoice ApplicationFeeId
instance StripeHasParam UpdateInvoice Closed
instance StripeHasParam UpdateInvoice Description
instance StripeHasParam UpdateInvoice Forgiven
instance StripeHasParam UpdateInvoice Metadata
instance StripeHasParam UpdateInvoice StatementDescriptor

------------------------------------------------------------------------------
-- | Pay `Invoice` by `InvoiceId`
payInvoice
    :: InvoiceId -- ^ The `InvoiceId` of the `Invoice` to pay
    -> StripeRequest PayInvoice
payInvoice
    invoiceid   = request
  where request = mkStripeRequest POST url params
        url     = "invoices" </> getInvoiceId invoiceid </> "pay"
        params  = []

data PayInvoice
type instance StripeReturn PayInvoice = Invoice
