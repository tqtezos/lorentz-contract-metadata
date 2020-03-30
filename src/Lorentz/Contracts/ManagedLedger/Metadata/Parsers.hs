
module Lorentz.Contracts.ManagedLedger.Metadata.Parsers where

import Data.Char
import Data.Bool
import Data.Either
import Data.String
import Data.Maybe
-- import Data.Semigroup
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read
import Text.Show
import Data.Function
import Data.Proxy
import qualified Text.ParserCombinators.ReadP as P

import qualified Tezos.Address as Tezos
import Lorentz (Address, View(..), GetDefaultEntryPointArg, NiceParameterFull, TAddress(..), callingDefTAddress) -- hiding ((>>))
import Prelude (FilePath, ($), Natural) -- hiding ((>>), fail)

import qualified Options.Applicative as Opt
import qualified Data.Text as T


-- Read Address

-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftM2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $
        ensureAddressPrefix >>
        P.munch1 isAlphaNum >>= \addressStr ->
          case Tezos.parseAddress $ T.pack addressStr of
            Left err -> fail $ show err
            Right address' -> return address'
  where
    ensureAddressPrefix =
      (do {('t':'z':'1':_) <- P.look; return ()}) <|>
      (do {('K':'T':'1':_) <- P.look; return ()})

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

-- END Read Address


-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " <> name <> "."
    ]

-- | Parse a `View` by parsing its arguments and @"callback-contract"@ address
parseView :: forall a r. NiceParameterFull r => Opt.Parser a -> Opt.Parser (View a (GetDefaultEntryPointArg r))
parseView parseArg =
  View <$> parseArg <*> fmap (callingDefTAddress . TAddress @r) (parseAddress "callback-contract")

-- | Parse a `View_`
parseView_ :: forall r. NiceParameterFull r => Proxy r -> Opt.Parser (View () (GetDefaultEntryPointArg r))
parseView_ _ = parseView @() @r $ pure ()

-- | Parse a `String`
parseString :: String -> Opt.Parser String
parseString name = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help $ "String representing the contract's initial " <> name <> "."
  ]


-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " <> name <> "."
    ]


-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse a natural number argument, given its field name
parseAuto :: Read a => String -> String -> Opt.Parser a
parseAuto name description =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar $ toUpper <$> name
    , Opt.help description
    ]

