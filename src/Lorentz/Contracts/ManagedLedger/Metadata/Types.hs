{-# LANGUAGE OverloadedStrings #-}

module Lorentz.Contracts.ManagedLedger.Metadata.Types (TokenId, TokenMetadata(..), mkTokenMetadata) where

import Lorentz

type TokenId = Natural

data TokenMetadata = TokenMetadata
     { metadata_tokenId :: Natural
     ,  metadata_rest1 :: TokenMetadataRest1
     }
  deriving stock Generic
  deriving anyclass IsoValue

data TokenMetadataRest1 = TokenMetadataRest1
     { metadata_symbol :: MText
     , metadata_rest2 :: TokenMetadataRest2
     }
  deriving stock Generic
  deriving anyclass IsoValue

data TokenMetadataRest2 = TokenMetadataRest2
     { metadata_name :: MText
     , metadata_rest3  :: TokenMetadataRest3
     }
  deriving stock Generic
  deriving anyclass IsoValue

data TokenMetadataRest3 = TokenMetadataRest3
     { metadata_decimals :: Natural
     , metadata_extras :: Map MText MText
     }
  deriving stock Generic
  deriving anyclass IsoValue

mkTokenMetadata :: Natural -> MText -> MText -> Natural -> Map MText MText -> TokenMetadata
mkTokenMetadata tokenid symbol name decimals extras =
  TokenMetadata
     { metadata_tokenId = tokenid
     ,  metadata_rest1 =
        TokenMetadataRest1
        { metadata_symbol = symbol
        , metadata_rest2 =
          TokenMetadataRest2
          { metadata_name = name
          , metadata_rest3  = TokenMetadataRest3
            { metadata_decimals = decimals
            , metadata_extras = extras }}}}
