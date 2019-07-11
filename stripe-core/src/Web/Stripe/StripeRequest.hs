{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Web.Stripe.StripeRequest
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.StripeRequest
  ( -- * Types
    Method(..)
  , Expandable(..)
  , ExpandParams(..)
  , Param(..)
  , Params
  , StripeRequest (..)
  , StripeReturn
  , StripeHasParam
  , ToStripeParam(..)
  , (-&-)
  , mkStripeRequest
  ) where

import           Data.ByteString                (ByteString)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))
import           Web.Stripe.Types               (ExpandParams (..),
                                                 Expandable (..))

------------------------------------------------------------------------------
-- | HTTP Method
--
-- The other methods are not required by the Stripe API
data Method
  = DELETE
  | GET
  | POST
    deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------
-- | HTTP Params
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | used to set a specific key/value pair when the type is not enough
newtype Param k v = Param (k, v)

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request. Also
-- includes the function needed to decode the response.
--
data StripeRequest a = StripeRequest
    { method      :: Method -- ^ Method of StripeRequest (i.e. `GET`, `PUT`, `POST`, `PUT`)
    , endpoint    :: Text   -- ^ Endpoint of StripeRequest
    , queryParams :: Params -- ^ Query Parameters of StripeRequest
    }

instance ToStripeParam (Param Text Text) where
  toStripeParam (Param (k,v)) =
    ((Text.encodeUtf8 k, Text.encodeUtf8 v) :)

------------------------------------------------------------------------------
-- | indicate if a request allows an optional parameter
class (ToStripeParam param) => StripeHasParam request param where

------------------------------------------------------------------------------
-- | add an optional parameter to a `StripeRequest`
(-&-) :: StripeHasParam request param =>
         StripeRequest request
      -> param
      -> StripeRequest request
stripeRequest -&- param =
  stripeRequest { queryParams =
                     toStripeParam param (queryParams stripeRequest)
                }

------------------------------------------------------------------------------
-- | return type of stripe request
type family StripeReturn a :: *

------------------------------------------------------------------------------
-- | HTTP Params
--
-- helper function for building a 'StripeRequest'
mkStripeRequest
    :: Method
    -> Text
    -> Params
    -> StripeRequest a
mkStripeRequest m e q = StripeRequest m e q
