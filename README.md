This adds FA2 style metadata to ManagedLedger.

This is the definition of metadata in Haskell:

``` haskell
data TokenMetadata = TokenMetadata
     { metadata_tokenId :: Natural
     ,  metadata_rest1 :: TokenMetadataRest1
     }

data TokenMetadataRest1 = TokenMetadataRest1
     { metadata_symbol :: MText
     , metadata_rest2 :: TokenMetadataRest2
     }

data TokenMetadataRest2 = TokenMetadataRest2
     { metadata_name :: MText
     , metadata_rest3  :: TokenMetadataRest3
     }

data TokenMetadataRest3 = TokenMetadataRest3
     { metadata_decimals :: Natural
     , metadata_extras :: Map MText MText
     }
```

This is the [view](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token_metadata):

```haskell
(pair
        (nat %token_id)
        (pair
          (string %symbol)
          (pair
            (string %name)
            (pair
              (nat %decimals)
              (map %extras string string)
      ))))
```
