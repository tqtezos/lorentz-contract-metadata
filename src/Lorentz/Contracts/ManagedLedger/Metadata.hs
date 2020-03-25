{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

-- | Managed ledger which is compatible with FA1.2 standard and
-- extended with administrator functionality.

module Lorentz.Contracts.ManagedLedger.Metadata
  ( Parameter (..)

  , Storage
  , mkStorage
  , managedLedgerMetadataContract

  , module Lorentz.Contracts.ManagedLedger.Types
  ) where

import Lorentz

import Lorentz.Contracts.ManagedLedger.Impl
import Lorentz.Contracts.ManagedLedger.Types

import Lorentz.Contracts.ManagedLedger.Metadata.Impl
import Lorentz.Contracts.ManagedLedger.Metadata.Types

import Lorentz.Contracts.ManagedLedger (mkStorage, Storage)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Parameter
----------------------------------------------------------------------------

data Parameter
  = Transfer         TransferParams
  | Approve          ApproveParams
  -- | ApproveCAS       ApproveCasParams
  | GetAllowance     (View GetAllowanceParams Natural)
  | GetBalance       (View GetBalanceParams Natural)
  | GetTotalSupply   (View () Natural)
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             MintParams
  | Burn             BurnParams
  | GetMetadata      (View [TokenId] [TokenMetadata])
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepPlain

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

managedLedgerMetadataContract :: TokenMetadata -> Contract Parameter Storage
managedLedgerMetadataContract metadata = do
  unpair
  caseT @Parameter
    ( #cTransfer /-> transfer
    , #cApprove /-> approve
    , #cGetAllowance /-> getAllowance
    , #cGetBalance /-> getBalance
    , #cGetTotalSupply /-> getTotalSupply
    , #cSetPause /-> setPause
    , #cSetAdministrator /-> setAdministrator
    , #cGetAdministrator /-> getAdministrator
    , #cMint /-> mint
    , #cBurn /-> burn
    , #cGetMetadata /-> getMetadata metadata
    )
