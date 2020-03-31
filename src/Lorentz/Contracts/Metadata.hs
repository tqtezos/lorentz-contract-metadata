
{-# LANGUAGE OverloadedStrings #-}

module Lorentz.Contracts.Metadata
  ( TokenId
  , TokenMetadata(..)
  , singleTokenTokenId
  , mkTokenMetadata
  , parseSingleTokenMetadata
  ) where

import Named
-- import Fmt (Buildable(..), genericF)
import qualified Options.Applicative as Opt

import Lorentz
import Michelson.Text
import Util.Named

import Lorentz.Contracts.ManagedLedger.Metadata.Parsers

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

-- | Construct `TokenMetadata` from @tokenid symbol name decimals extras@
mkTokenMetadata ::
     ("tokenId" :! TokenId)
  -> ("symbol" :! MText)
  -> ("name" :! MText)
  -> ("decimals" :! Natural)
  -> ("extras" :! Map MText MText)
  -> TokenMetadata
mkTokenMetadata tokenid symbol name decimals extras =
  TokenMetadata
    { metadata_tokenId = arg #tokenId tokenid
    , metadata_rest1 =
        TokenMetadataRest1
          { metadata_symbol = arg #symbol symbol
          , metadata_rest2 =
              TokenMetadataRest2
                { metadata_name = arg #name name
                , metadata_rest3 =
                    TokenMetadataRest3
                      { metadata_decimals = arg #decimals decimals
                      , metadata_extras = arg #extras extras
                      }
                }
          }
    }

-- | The `TokenId` of a token in a single-token contract is @0@
singleTokenTokenId :: TokenId
singleTokenTokenId = 0

-- | Parse `TokenMetadata` for a single token, with no extras
parseSingleTokenMetadata :: Opt.Parser TokenMetadata
parseSingleTokenMetadata =
  mkTokenMetadata <$>
  pure (#tokenId .! singleTokenTokenId) <*>
  ((#symbol .!) . mkMTextUnsafe . fromString <$> parseString "token-symbol") <*>
  ((#name .!) . mkMTextUnsafe . fromString <$> parseString "token-name") <*>
  ((#decimals .!) <$> parseNatural "token-decimals") <*>
  pure (#extras .! mempty)
