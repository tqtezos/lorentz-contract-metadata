{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

-- | Implementation of managed ledger which does not require
-- particular storage type.

module Lorentz.Contracts.ManagedLedger.Metadata.Impl (getMetadata) where

import Prelude hiding (drop, (>>))

import Michelson.Text

import Lorentz

import Lorentz.Contracts.ManagedLedger.Impl ()

import Lorentz.Contracts.ManagedLedger.Types (LedgerValue)
import Lorentz.Contracts.Metadata

type LedgerC store = StorageContains store
  [ "totalSupply" := Natural
  , "ledger" := Address ~> LedgerValue
  ]

getMetadata :: LedgerC store => TokenMetadata -> Entrypoint (View [TokenId] [TokenMetadata]) store
getMetadata tokenMetadata = view_ $ do
  car
  assertSingleTokenIds
  push [tokenMetadata]

assertSingleTokenIds :: [TokenId] & s :-> s
assertSingleTokenIds = do
  iter $ do
    push 0
    assertEq (mkMTextUnsafe "single token only has id 0")
