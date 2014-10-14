{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Dispute
    ( -- * API
      updateDispute
    , closeDispute
      -- * Types
    , ChargeId      (..)
    , Dispute       (..)
    , DisputeReason (..)
    , DisputeStatus (..)
    , Evidence      (..)
    ) where

import           Web.Stripe.Client.Internal (Method (POST), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, (</>), toMetaData)
import           Web.Stripe.Types           (ChargeId (..), Dispute (..),
                                             DisputeReason (..),
                                             DisputeStatus (..),
                                             Evidence (..), MetaData)
import           Web.Stripe.Types.Util      (getChargeId)

------------------------------------------------------------------------------
-- | `Dispute` to be updated
updateDispute
    :: ChargeId        -- ^ The ID of the Charge being disputed
    -> Maybe Evidence  -- ^ Text-only evidence of the dispute
    -> MetaData        -- ^ `MetaData` associated with `Dispute`
    -> Stripe Dispute
updateDispute
    chargeId
    evidence
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute"
        params  = toMetaData metadata ++ getParams [
                   ("evidence", (\(Evidence x) -> x) `fmap` evidence)
                  ]
------------------------------------------------------------------------------
-- | `Dispute` to be closed
closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> Stripe Dispute
closeDispute
    chargeId = callAPI request
  where request = StripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute" </> "close"
        params  = []
