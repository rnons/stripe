{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Web.Stripe.Types.TimeRange where

import           Data.ByteString                (ByteString)
import           Data.Data                      (Data, Typeable)
import           Web.Stripe.StripeRequest.Class (ToStripeParam (..))

------------------------------------------------------------------------------
-- | specify a `TimeRange`
-- FIXME: this is a little awkward to use. How can we make it moar better?
data TimeRange a = TimeRange
    { gt  :: Maybe a
    , gte :: Maybe a
    , lt  :: Maybe a
    , lte :: Maybe a
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

instance ToStripeParam a => ToStripeParam (TimeRange a) where
  toStripeParam (TimeRange{..}) =
    (case gt of
      Nothing -> id
      Just t  -> toRecord (toStripeParam t) "gt") .
    (case gte of
      Nothing -> id
      Just t  -> toRecord (toStripeParam t) "gte") .
    (case lt of
      Nothing -> id
      Just t  -> toRecord (toStripeParam t) "lt") .
    (case lte of
      Nothing -> id
      Just t  -> toRecord (toStripeParam t) "lte")
    where
      toRecord :: ([(ByteString, ByteString)] -> [(ByteString, ByteString)])
               -> ByteString
               -> ([(ByteString, ByteString)] -> [(ByteString, ByteString)])
      toRecord f n =
        case f [] of
          [(k,v)] -> ((k <> "[" <> n <> "]", v) :)
          lst'       -> error $ "toRecord in ToStripeRange (TimeRange a) expected exactly one element in this list. " ++ show lst'

------------------------------------------------------------------------------
-- | Time range with all values set to `Nothing`
emptyTimeRange :: TimeRange a
emptyTimeRange = TimeRange
    { gt  = Nothing
    , gte = Nothing
    , lt  = Nothing
    , lte = Nothing
    }
