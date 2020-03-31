This adds FA2 style metadata to ManagedLedger.

# Installation instructions

```bash
git clone https://github.com/tqtezos/lorentz-contract-metadata.git
cd lorentz-contract-metadata
stack install
```


# Metadata definition

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




# CLI Usage

```bash
❯❯❯ stack exec -- lorentz-contract-metadata --help
Lorentz tools

Usage: lorentz-contract-metadata COMMAND
  Sale contract parameter generation helper

Available options:
  -h,--help                Show this help text

Available commands:
  Metadata                 Whitelist contract CLI interface

You can use help for specific COMMAND
EXAMPLE:
  lorentz-contract-metadata COMMAND --help
```

`ManagedLedger` `Metadata`:

```bash
❯❯❯ stack exec -- lorentz-contract-metadata Metadata --help
Usage: lorentz-contract-metadata Metadata COMMAND
  Whitelist contract CLI interface

Available options:
  -h,--help                Show this help text

Available commands:
  print                    Dump the ManagedLedger Metadata contract in form of
                           Michelson code
  init                     Initial storage for the ManagedLedger Metadata
                           contract
  getMetadata              GetMetadata parameter for the ManagedLedger Metadata
                           contract
  burn                     Burn parameter for the ManagedLedger Metadata
                           contract
  mint                     Mint parameter for the ManagedLedger Metadata
                           contract
  getAdministrator         GetAdministrator parameter for the ManagedLedger
                           Metadata contract
  setAdministrator         SetAdministrator parameter for the ManagedLedger
                           Metadata contract
  setPause                 SetPause parameter for the ManagedLedger Metadata
                           contract
  getTotalSupply           GetTotalSupply parameter for the ManagedLedger
                           Metadata contract
  getBalance               GetBalance parameter for the ManagedLedger Metadata
                           contract
  getAllowance             GetAllowance parameter for the ManagedLedger Metadata
                           contract
  approve                  Approve parameter for the ManagedLedger Metadata
                           contract
  transfer                 Transfer parameter for the ManagedLedger Metadata
                           contract
```

To print the contract:

```bash
❯❯❯ stack exec -- lorentz-contract-metadata Metadata print --token-symbol "TOK" --token-name "Token" --token-decimals 0 --oneline
parameter (or (or (or (pair %transfer (address :from) (pair (address :to) (nat :value))) (pair %approve (address :spender) (nat :value))) (or (pair %getAllowance (pair (address :owner) (address :spender)) (contract nat)) (or (pair %getBalance (address :owner) (contract nat)) (pair %getTotalSupply unit (contract nat))))) (or (or (bool %setPause) (or (address %setAdministrator) (pair %getAdministrator unit (contract address)))) (or (pair %mint (address :to) (nat :value)) (or (pair %burn (address :from) (nat :value)) (pair %getMetadata (list nat) (contract (list (pair nat (pair string (pair string (pair nat (map string string))))))))))));storage (pair (big_map address (pair nat (map address nat))) (pair address (pair bool nat)));code { CAST (pair (or (or (or (pair address (pair address nat)) (pair address nat)) (or (pair (pair address address) (contract nat)) (or (pair address (contract nat)) (pair unit (contract nat))))) (or (or bool (or address (pair unit (contract address)))) (or (pair address nat) (or (pair address nat) (pair (list nat) (contract (list (pair nat (pair string (pair string (pair nat (map string string)))))))))))) (pair (big_map address (pair nat (map address nat))) (pair address (pair bool nat))));DUP;CAR;DIP { CDR };IF_LEFT { IF_LEFT { IF_LEFT { DIP { DUP;CDR;CDR;CAR;IF { UNIT;PUSH string "TokenOperationsArePaused";PAIR;FAILWITH }   {  } };DUP;DUP;CDR;CAR;DIP { CAR };COMPARE;EQ;IF { DROP }   { DUP;CAR;SENDER;COMPARE;EQ;IF {  }   { DUP;DIP { DUP;DIP { DIP { DUP };CAR;SENDER;PAIR;DUP;DIP { CDR;DIP { CAR };GET;IF_NONE { EMPTY_MAP (address) nat }        { CDR } };CAR;GET;IF_NONE { PUSH nat 0 }        {  } };DUP;CAR;DIP { SENDER;DIP { DUP;CDR;CDR;DIP 2 { DUP };DIG 2;SUB;ISNAT;IF_NONE { DIP { DUP };SWAP;DIP { DUP };SWAP;CDR;CDR;PAIR;PUSH string "NotEnoughAllowance";PAIR;FAILWITH }        {  } };PAIR };PAIR;DIP { DROP;DROP };DIP { DUP };SWAP;DIP { DUP;CAR };SWAP;DIP { CAR };GET;IF_NONE { PUSH nat 0;DIP { EMPTY_MAP (address) nat };PAIR;EMPTY_MAP (address) nat }        { DUP;CDR };DIP 2 { DUP };DIG 2;CDR;CDR;DUP;INT;EQ;IF { DROP;NONE nat }   { SOME };DIP 3 { DUP };DIG 3;CDR;CAR;UPDATE;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;CAR;DIP { SOME };DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR } };DIP { DUP };SWAP;DIP { DUP };SWAP;CDR;CAR;DIP { CAR };GET;IF_NONE { DUP;CDR;CDR;INT;EQ;IF { NONE (pair nat (map address nat)) }   { DUP;CDR;CDR;DIP { EMPTY_MAP (address) nat };PAIR;SOME } }        { DIP { DUP };SWAP;CDR;CDR;DIP { DUP;CAR };ADD;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;SOME };SWAP;DUP;DIP { CDR;CAR;DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR };DUP;DIP { CDR;CDR;INT;DIP { DUP;CDR;CDR;CDR };ADD;ISNAT;IF_NONE { PUSH string "Internal: Negative total supply";FAILWITH }        {  };DIP { DUP;CDR };DIP { DUP;DIP { CAR };CDR };DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR };DIP { DUP };SWAP;DIP { DUP };SWAP;CAR;DIP { CAR };GET;IF_NONE { CDR;CDR;PUSH nat 0;SWAP;PAIR;PUSH string "NotEnoughBalance";PAIR;FAILWITH }        {  };DUP;CAR;DIP 2 { DUP };DIG 2;CDR;CDR;SWAP;SUB;ISNAT;IF_NONE { CAR;DIP { DUP };SWAP;CDR;CDR;PAIR;PUSH string "NotEnoughBalance";PAIR;FAILWITH }        {  };DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;DIP { DUP };SWAP;DIP { DUP;CAR;INT;EQ;IF { DUP;CDR;SIZE;INT;EQ;IF { DROP;NONE (pair nat (map address nat)) }   { SOME } }   { SOME };SWAP;CAR;DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR };DUP;DIP { CDR;CDR;NEG;DIP { DUP;CDR;CDR;CDR };ADD;ISNAT;IF_NONE { PUSH string "Internal: Negative total supply";FAILWITH }        {  };DIP { DUP;CDR };DIP { DUP;DIP { CAR };CDR };DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR };DROP };NIL operation;PAIR }        { SENDER;PAIR;DIP { DUP;CDR;CDR;CAR;IF { UNIT;PUSH string "TokenOperationsArePaused";PAIR;FAILWITH }   {  } };DIP { DUP };SWAP;DIP { DUP };SWAP;DUP;DIP { CAR;DIP { CAR };GET;IF_NONE { EMPTY_MAP (address) nat }        { CDR } };CDR;CAR;GET;IF_NONE { PUSH nat 0 }        {  };DUP;INT;EQ;IF { DROP }   { DIP { DUP };SWAP;CDR;CDR;INT;EQ;IF { DROP }   { PUSH string "UnsafeAllowanceChange";PAIR;FAILWITH } };DIP { DUP };SWAP;DIP { DUP;CAR };SWAP;DIP { CAR };GET;IF_NONE { PUSH nat 0;DIP { EMPTY_MAP (address) nat };PAIR;EMPTY_MAP (address) nat }        { DUP;CDR };DIP 2 { DUP };DIG 2;CDR;CDR;DUP;INT;EQ;IF { DROP;NONE nat }   { SOME };DIP 3 { DUP };DIG 3;CDR;CAR;UPDATE;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;CAR;DIP { SOME };DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;NIL operation;PAIR } }        { IF_LEFT { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;DUP;CAR;DIP { CDR };DUP;DIP { CAR;DIP { CAR };GET;IF_NONE { EMPTY_MAP (address) nat }        { CDR } };CDR;GET;IF_NONE { PUSH nat 0 }        {  };DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR }        { IF_LEFT { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;DUP;CAR;DIP { CDR };DIP { CAR };GET;IF_NONE { PUSH nat 0 }        { CAR };DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR }        { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;CDR;CDR;CDR;CDR;DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR } } } }        { IF_LEFT { IF_LEFT { DIP { DUP;CDR;CAR;SENDER;COMPARE;EQ;IF {  }   { UNIT;PUSH string "SenderIsNotAdmin";PAIR;FAILWITH } };DIP { DUP;CDR };DIP { DUP;DIP { CAR };CDR };DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;SWAP;PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;NIL operation;PAIR }        { IF_LEFT { DIP { DUP;CDR;CAR;SENDER;COMPARE;EQ;IF {  }   { UNIT;PUSH string "SenderIsNotAdmin";PAIR;FAILWITH } };DIP { DUP;CDR };DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;NIL operation;PAIR }        { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;CDR;CDR;CAR;DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR } } }        { IF_LEFT { DIP { DUP;CDR;CAR;SENDER;COMPARE;EQ;IF {  }   { UNIT;PUSH string "SenderIsNotAdmin";PAIR;FAILWITH } };DIP { DUP };SWAP;DIP { DUP };SWAP;CAR;DIP { CAR };GET;IF_NONE { DUP;CDR;INT;EQ;IF { NONE (pair nat (map address nat)) }   { DUP;CDR;DIP { EMPTY_MAP (address) nat };PAIR;SOME } }        { DIP { DUP };SWAP;CDR;DIP { DUP;CAR };ADD;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;SOME };SWAP;DUP;DIP { CAR;DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR };DUP;DIP { CDR;INT;DIP { DUP;CDR;CDR;CDR };ADD;ISNAT;IF_NONE { PUSH string "Internal: Negative total supply";FAILWITH }        {  };DIP { DUP;CDR };DIP { DUP;DIP { CAR };CDR };DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR };DROP;NIL operation;PAIR }        { IF_LEFT { DIP { DUP;CDR;CAR;SENDER;COMPARE;EQ;IF {  }   { UNIT;PUSH string "SenderIsNotAdmin";PAIR;FAILWITH } };DIP { DUP };SWAP;DIP { DUP };SWAP;CAR;DIP { CAR };GET;IF_NONE { CDR;PUSH nat 0;SWAP;PAIR;PUSH string "NotEnoughBalance";PAIR;FAILWITH }        {  };DUP;CAR;DIP 2 { DUP };DIG 2;CDR;SWAP;SUB;ISNAT;IF_NONE { CAR;DIP { DUP };SWAP;CDR;PAIR;PUSH string "NotEnoughBalance";PAIR;FAILWITH }        {  };DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR;DIP { DUP };SWAP;DIP { DUP;CAR;INT;EQ;IF { DUP;CDR;SIZE;INT;EQ;IF { DROP;NONE (pair nat (map address nat)) }   { SOME } }   { SOME };SWAP;CAR;DIP { DIP { DUP;CAR } };UPDATE;DIP { DUP;DIP { CDR };CAR };DIP { DROP };PAIR };DUP;DIP { CDR;NEG;DIP { DUP;CDR;CDR;CDR };ADD;ISNAT;IF_NONE { PUSH string "Internal: Negative total supply";FAILWITH }        {  };DIP { DUP;CDR };DIP { DUP;DIP { CAR };CDR };DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR;SWAP;PAIR;DIP { DUP;DIP { CAR };CDR };DIP { DROP };SWAP;PAIR };DROP;NIL operation;PAIR }        { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;CAR;ITER { PUSH nat 0;COMPARE;EQ;IF {  }   { PUSH string "single token only has id 0";FAILWITH } };PUSH (list (pair nat (pair string (pair string (pair nat (map string string)))))) { Pair 0 (Pair "TOK" (Pair "Token" (Pair 0 { }))) };DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR } } } } };
```





