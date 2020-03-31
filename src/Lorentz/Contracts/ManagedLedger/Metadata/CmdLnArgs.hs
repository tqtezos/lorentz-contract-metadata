{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.ManagedLedger.Metadata.CmdLnArgs where

import Control.Applicative
import Data.Functor
import Prelude (FilePath, IO)
import Data.Maybe

import Lorentz
import Util.IO
import Util.Named

import qualified Options.Applicative as Opt
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map as Map

import Lorentz.Contracts.ManagedLedger.Metadata.Parsers
import qualified Lorentz.Contracts.ManagedLedger.Types as Metadata
import qualified Lorentz.Contracts.ManagedLedger.Metadata as Metadata
import Lorentz.Contracts.Metadata


data CmdLnArgs
  = Print            TokenMetadata (Maybe FilePath) Bool
  | Init             Metadata.Storage
  | Transfer         Metadata.TransferParams
  | Approve          Metadata.ApproveParams
  | GetAllowance     (View Metadata.GetAllowanceParams Natural)
  | GetBalance       (View Metadata.GetBalanceParams Natural)
  | GetTotalSupply   (View () Natural)
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             Metadata.MintParams
  | Burn             Metadata.BurnParams
  | GetMetadata      (View [TokenId] [TokenMetadata])

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  , getMetadataSubCmd
  , burnSubCmd
  , mintSubCmd
  , getAdministratorSubCmd
  , setAdministratorSubCmd
  , setPauseSubCmd
  , getTotalSupplySubCmd
  , getBalanceSubCmd
  , getAllowanceSubCmd
  , approveSubCmd
  , transferSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$>
        parseSingleTokenMetadata <*>
        outputOptions <*>
        onelineOption)
      "Dump the ManagedLedger Metadata contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        (Metadata.mkStorage <$>
          parseAddress "admin" <*>
          fmap Map.fromList (parseAuto @[(Address, Natural)]
            "initial-ledger"
            "A list of tuples of (Address, Natural), representing the initial ledger"
                 ))
      )
      "Initial storage for the ManagedLedger Metadata contract"

    transferSubCmd =
      mkCommandParser "transfer"
      (Transfer <$>
        ((,,) <$>
          ((#from .!) <$> parseAddress "from") <*>
          ((#to .!) <$> parseAddress "to") <*>
          ((#value .!) <$> parseNatural "value")
        )
      )
      "Transfer parameter for the ManagedLedger Metadata contract"

    approveSubCmd =
      mkCommandParser "approve"
      (Approve <$>
        ((,) <$>
          ((#spender .!) <$> parseAddress "spender") <*>
          ((#value .!) <$> parseNatural "value")
        )
      )
      "Approve parameter for the ManagedLedger Metadata contract"

    getAllowanceSubCmd =
      mkCommandParser "getAllowance"
      (GetAllowance <$> parseView @_ @Natural
        ((,) <$>
          ((#owner .!) <$> parseAddress "owner") <*>
          ((#spender .!) <$> parseAddress "spender")
        )
      )
      "GetAllowance parameter for the ManagedLedger Metadata contract"

    getBalanceSubCmd =
      mkCommandParser "getBalance"
      (GetBalance <$> parseView @_ @Natural
        ((#owner .!) <$> parseAddress "owner")
      )
      "GetBalance parameter for the ManagedLedger Metadata contract"

    getTotalSupplySubCmd =
      mkCommandParser "getTotalSupply"
      (GetTotalSupply <$> parseView_ (Proxy @Natural)
      )
      "GetTotalSupply parameter for the ManagedLedger Metadata contract"

    setPauseSubCmd =
      mkCommandParser "setPause"
      (SetPause <$> parseBool "paused"
      )
      "SetPause parameter for the ManagedLedger Metadata contract"

    setAdministratorSubCmd =
      mkCommandParser "setAdministrator"
      (SetAdministrator <$> parseAddress "new-admin"
      )
      "SetAdministrator parameter for the ManagedLedger Metadata contract"

    getAdministratorSubCmd =
      mkCommandParser "getAdministrator"
      (GetAdministrator <$> parseView_ (Proxy @Address)
      )
      "GetAdministrator parameter for the ManagedLedger Metadata contract"

    mintSubCmd =
      mkCommandParser "mint"
      (Mint <$>
        ((,) <$>
          ((#to .!) <$> parseAddress "to") <*>
          ((#value .!) <$> parseNatural "value")
        )
      )
      "Mint parameter for the ManagedLedger Metadata contract"

    burnSubCmd =
      mkCommandParser "burn"
      (Burn <$>
        ((,) <$>
          ((#from .!) <$> parseAddress "from") <*>
          ((#value .!) <$> parseNatural "value")
        )
      )
      "Burn parameter for the ManagedLedger Metadata contract"

    getMetadataSubCmd =
      mkCommandParser "getMetadata"
      (GetMetadata <$> parseView @_ @[TokenMetadata] (pure [singleTokenTokenId]))
      "GetMetadata parameter for the ManagedLedger Metadata contract"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Whitelist contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print tokenMetadata' mOutput forceOneLine ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine $
    Metadata.managedLedgerMetadataContract tokenMetadata'
  Init xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  Transfer xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  Approve xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  GetAllowance xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  GetBalance xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  GetTotalSupply xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  SetPause xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  SetAdministrator xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  GetAdministrator xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  Mint xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  Burn xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  GetMetadata xs -> TL.putStrLn . printLorentzValue forceSingleLine $ xs
  where
    forceSingleLine = True


