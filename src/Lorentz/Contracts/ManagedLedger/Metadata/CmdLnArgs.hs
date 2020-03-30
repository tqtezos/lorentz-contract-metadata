{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.ManagedLedger.Metadata.CmdLnArgs where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Data.Functor
import Prelude (FilePath, IO, Ord(..))
import Data.String (IsString(..), String)
import Data.Maybe
import Data.Typeable
import Text.Read

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Typed.Annotation
import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.IO
import qualified Michelson.Untyped.Type as U

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map as Map
import Data.Constraint
import Data.Singletons

-- import Lorentz.Contracts.Util ()
-- import Lorentz.Contracts.SomeContractParam
-- import Lorentz.Contracts.Parse
-- import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

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

  -- | Transfer !Metadata.TransferParams
  -- | SetIssuer
  --     { newIssuer :: !SomeContractParam
  --     , wrapped :: !Bool
  --     }
  -- | AddUser
  --     { newUser :: !SomeContractParam
  --     , newUserWhitelistId :: !(Maybe WhitelistId)
  --     , wrapped :: !Bool
  --     }
  -- | SetWhitelistOutbound
  --     { whitelistOutboundParams :: !WhitelistOutboundParams
  --     , wrapped :: !Bool
  --     }
  -- | SetAdmin
  --     { admin :: !Address
  --     , wrapped :: !Bool
  --     }
  -- | GetIssuer
  --     { viewIssuer :: !(View_ ())
  --     , wrapped :: !Bool
  --     }
  -- | GetUser
  --     { viewUser :: !(View SomeContractParam (Maybe WhitelistId))
  --     , wrapped :: !Bool
  --     }
  -- | GetWhitelist
  --     { viewWhitelist :: !(View WhitelistId (Maybe OutboundWhitelists))
  --     , wrapped :: !Bool
  --     }
  -- | GetAdmin
  --     { viewAdmin :: !(View_ Address)
  --     , wrapped :: !Bool
  --     }
  -- | WrappedParam
  --     { wrappedParam :: !SomeContractParam
  --     }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  -- , assertTransferSubCmd
  -- , setIssuerSubCmd
  -- , addUserSubCmd
  -- , setWhitelistOutboundSubCmd
  -- , setAdminSubCmd
  -- , getIssuerSubCmd
  -- , getUserSubCmd
  -- , getWhitelistSubCmd
  -- , getAdminSubCmd
  -- , wrappedParamSubCmd
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

    -- assertTransferSubCmd =
    --   mkCommandParser "assertTransfer"
    --   (AssertTransfer <$> parseSomeTransferParams)
    --   "Generate the parameter for the Whitelist contract: AssertTransfer"

    -- setIssuerSubCmd =
    --   mkCommandParser "SetIssuer"
    --   (SetIssuer <$>
    --     parseSomeContractParam "newIssuer" <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: SetIssuer"

    -- addUserSubCmd =
    --   mkCommandParser "AddUser"
    --   (AddUser <$>
    --     parseSomeContractParam "newUser" <*>
    --     parseMaybe (parseNatural "newUserWhitelistId") <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: AddUser"

    -- setWhitelistOutboundSubCmd =
    --   mkCommandParser "SetWhitelistOutbound"
    --   (SetWhitelistOutbound <$>
    --     (WhitelistOutboundParams <$>
    --       parseNatural "whitelistId" <*>
    --       (parseMaybe (Whitelist.mkOutboundWhitelists <$>
    --         parseBool "restricted" <*>
    --         Opt.option
    --           (parseList Opt.auto)
    --           (mconcat
    --             [ Opt.long "outboundWhitelist"
    --             , Opt.metavar "[Natural]"
    --             , Opt.help "List of allowed outbound whitelists"
    --             ]
    --           )
    --       ))
    --     ) <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: SetWhitelistOutbound"

    -- setAdminSubCmd =
    --   mkCommandParser "SetAdmin"
    --   (SetAdmin <$>
    --     parseAddress "admin" <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: SetAdmin"

    -- getIssuerSubCmd =
    --   mkCommandParser "GetIssuer"
    --   (GetIssuer <$>
    --     parseView_ <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: GetIssuer"

    -- getUserSubCmd =
    --   mkCommandParser "GetUser"
    --   (GetUser <$>
    --     parseView (parseSomeContractParam "user") <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: GetUser"

    -- getWhitelistSubCmd =
    --   mkCommandParser "GetWhitelist"
    --   (GetWhitelist <$>
    --     parseView (parseNatural "whitelistId") <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: GetWhitelist"

    -- getAdminSubCmd =
    --   mkCommandParser "GetAdmin"
    --   (GetAdmin <$>
    --     parseView_ <*>
    --     parseBool "wrapped"
    --   )
    --   "Generate the (wrapped) parameter for the Whitelist contract: GetAdmin"

    -- wrappedParamSubCmd =
    --   mkCommandParser "WrappedParam"
    --   (WrappedParam <$> parseSomeContractParam "wrappedParam")
    --   ("Generate a wrapped parameter for the Whitelist contract, given the " <>
    --   "original contract's parameter")

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Whitelist contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  -- Print (SomeSing (st :: Sing t)) mOutput forceOneLine ->
  --   withDict (singIT st) $
  --   withDict (singTypeableT st) $
  --   assertOpAbsense @t $
  --   assertBigMapAbsense @t $
  --   assertIsComparable @t $
  --   withDict (compareOpCT @(ToCT (Value t))) $
  --   maybe TL.putStrLn writeFileUtf8 mOutput $
  --   printLorentzContract forceOneLine (Whitelist.whitelistContract @(Value t))
  -- Init {..} ->
  --   fromSomeStorage initialStorage $ \(initialStorage' :: Whitelist.Storage (Value s)) ->
  --     assertIsComparable @s $
  --     case initialWrappedStorage of
  --       Nothing ->
  --         TL.putStrLn . printLorentzValue @(Whitelist.Storage (Value s)) forceSingleLine $
  --         initialStorage'
  --       Just initialWrappedStorage' ->
  --         fromSomeContractStorage initialWrappedStorage' $ \(initialWrappedStorage'' :: Value t) ->
  --         let st = sing @t in
  --         withDict (singIT st) $
  --         withDict (singTypeableT st) $
  --         TL.putStrLn $
  --         printLorentzValue @(Wrapper.Storage (Value t) (Value s)) forceSingleLine $
  --         Wrapper.Storage
  --           initialWrappedStorage''
  --           initialStorage'
  -- AssertTransfer {..} ->
  --   fromSomeTransferParams assertTransferParams $ \(assertTransferParams' :: Whitelist.TransferParams (Value t)) ->
  --   TL.putStrLn . printLorentzValue forceSingleLine $
  --   Whitelist.AssertTransfer assertTransferParams'
  -- SetIssuer {..} ->
  --   fromSomeContractParam newIssuer $ \(newIssuer' :: Value t) ->
  --     let st = sing @t in
  --     withDict (singIT st) $
  --     withDict (singTypeableT st) $
  --     if wrapped
  --        then
  --          TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
  --          Wrapper.WhitelistParameter $
  --          Whitelist.SetIssuer newIssuer'
  --        else
  --          TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
  --          Whitelist.OtherParameter $
  --          Whitelist.SetIssuer newIssuer'
  -- AddUser {..} ->
  --   fromSomeContractParam newUser $ \(newUser' :: Value t) ->
  --     let st = sing @t in
  --     withDict (singIT st) $
  --     withDict (singTypeableT st) $
  --     if wrapped
  --        then
  --          TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
  --          Wrapper.WhitelistParameter $
  --          Whitelist.AddUser $
  --          Whitelist.UpdateUserParams newUser' newUserWhitelistId
  --        else
  --          TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
  --          Whitelist.OtherParameter $
  --          Whitelist.AddUser $
  --          Whitelist.UpdateUserParams newUser' newUserWhitelistId
  -- SetWhitelistOutbound {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.SetWhitelistOutbound whitelistOutboundParams
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.SetWhitelistOutbound whitelistOutboundParams
  -- SetAdmin {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.SetAdmin admin
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.SetAdmin admin
  -- GetIssuer {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.GetIssuer viewIssuer
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.GetIssuer viewIssuer
  -- GetUser {..} ->
  --   case viewUser of
  --     View viewParam addr' ->
  --       fromSomeContractParam viewParam $ \(viewParam' :: Value t) ->
  --         let viewUser' = View viewParam' addr'
  --          in if wrapped
  --                then
  --                  TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
  --                  Wrapper.WhitelistParameter $
  --                  Whitelist.GetUser viewUser'
  --                else
  --                  TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
  --                  Whitelist.OtherParameter $
  --                  Whitelist.GetUser viewUser'
  -- GetWhitelist {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.GetWhitelist viewWhitelist
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.GetWhitelist viewWhitelist
  -- GetAdmin {..} ->
  --   if wrapped
  --      then
  --        TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
  --        Wrapper.WhitelistParameter $
  --        Whitelist.GetAdmin viewAdmin
  --      else
  --        TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
  --        Whitelist.OtherParameter $
  --        Whitelist.GetAdmin viewAdmin
  -- WrappedParam {..} ->
  --   fromSomeContractParam wrappedParam $ \(wrappedParam' :: Value t) ->
  --     let st = sing @t in
  --     withDict (singIT st) $
  --     withDict (singTypeableT st) $
  --     TL.putStrLn . printLorentzValue @(Wrapper.Parameter (Value t) ()) forceSingleLine $
  --     Wrapper.WrappedParameter wrappedParam'
  -- where
  --   forceSingleLine = True


