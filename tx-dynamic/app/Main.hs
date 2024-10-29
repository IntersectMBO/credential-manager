module Main where

import Cardano.Api (
  AsType (..),
  Doc,
  Error (prettyError),
  File (..),
  FromSomeType (FromSomeType),
  Hash,
  InAnyShelleyBasedEra (InAnyShelleyBasedEra),
  Key (..),
  PaymentKey,
  SerialiseAsRawBytes,
  ShelleyBasedEra (..),
  ToCardanoEra (toCardanoEra),
  TxBody (..),
  TxBodyContent (..),
  TxExtraKeyWitnesses (..),
  TxId,
  deserialiseFromRawBytesHex,
  getTxBody,
  readFileTextEnvelope,
  readFileTextEnvelopeAnyOf,
  serialiseToRawBytesHexText,
 )
import Cardano.CLI.Json.Friendly (
  friendlyTxBody,
  viewOutputFormatToFriendlyFormat,
 )
import Cardano.CLI.Types.Common (ViewOutputFormat (..))
import Cardano.Crypto.DSIGN (SigDSIGN (..))
import Cardano.Crypto.PinnedSizedBytes (psbToByteString)
import Cardano.TxDynamic (
  GroupError (..),
  GroupHeader (..),
  GroupMemHeader (..),
  SignBundleError (..),
  SomeTxBundle (..),
  TxBundle (..),
  WitnessBundle (..),
  signBundle,
  signatureCombinations,
 )
import Control.Monad (foldM, guard, join, unless)
import Data.Base16.Types (extractBase16)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (decodeFile, encodeFile)
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (Foldable (..), asum, for_)
import Data.Functor (void)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import Data.Version (showVersion)
import Data.Word (Word16)
import Options.Applicative (
  Alternative (..),
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  auto,
  eitherReader,
  execParser,
  flag,
  flag',
  fullDesc,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
 )
import Options.Applicative.Builder (command)
import Options.Applicative.Extra (hsubparser)
import Paths_tx_dynamic (version)
import Prettyprinter (
  Pretty (..),
  brackets,
  comma,
  emptyDoc,
  encloseSep,
  hang,
  indent,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (AnsiStyle, hPutDoc, putDoc)
import System.Directory.Internal.Prelude (exitFailure, hPutStrLn)
import System.Exit (die)
import System.IO (stderr)
import Text.Printf (printf)

main :: IO ()
main = do
  join . execParser . info (helper <*> versionOption <*> rootParser) . fold $
    [ fullDesc
    , progDesc
        "tx-bundle: a command line utility for interacting with transaction bundle files"
    ]

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("tx-dynamic " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

rootParser :: Parser (IO ())
rootParser =
  hsubparser $
    fold
      [ command "build" $ runBuildCommand <$> buildCommandParser
      , command "witness" $ runWitnessCommand <$> witnessCommandParser
      , command "info" $ runInfoCommand <$> infoCommandParser
      , command "witness-info" $ runWitnessInfoCommand <$> witnessInfoCommandParser
      ]

-- * Build command

data Group = Group
  { groupThreshold :: Word16
  , groupMembers :: [Hash PaymentKey]
  }
  deriving (Show, Eq)

data BuildCommand = BuildCommand
  { bcTxBodyFile :: FilePath
  , bcGroups :: [Group]
  , bcOutFile :: FilePath
  }
  deriving (Show)

buildCommandParser :: ParserInfo BuildCommand
buildCommandParser = info parser description
  where
    parser =
      BuildCommand
        <$> txBodyFileParser
        <*> groupsParser
        <*> outFileParser "transaction bundle"
    description = progDesc "Build a transaction bundle signable by groups of signatories"

outFileParser :: String -> Parser FilePath
outFileParser assetType =
  strOption . fold $
    [ long "out-file"
    , metavar "FILE"
    , help $ printf "File to which to write the %s" assetType
    ]

groupsParser :: Parser [Group]
groupsParser = nub <$> some groupParser

groupParser :: Parser Group
groupParser = Group <$> groupThresholdParser <*> fmap nub (some groupMemberParser)

groupThresholdParser :: Parser Word16
groupThresholdParser =
  option auto . fold $
    [ long "group-threshold"
    , metavar "INT"
    , help
        "Starts a new group. The minimum number of signers from the group that must sign the transaction"
    ]

groupMemberParser :: Parser (Hash PaymentKey)
groupMemberParser =
  hexOption (AsHash AsPaymentKey) . fold $
    [ long "verification-key-hash"
    , metavar "HEX"
    , help "The verification key hash of a group member"
    ]

hexOption
  :: (SerialiseAsRawBytes a) => AsType a -> Mod OptionFields a -> Parser a
hexOption asType =
  option $
    eitherReader $
      first show . deserialiseFromRawBytesHex asType . T.encodeUtf8 . T.pack

txBodyFileParser :: Parser FilePath
txBodyFileParser =
  strOption . fold $
    [ long "tx-body-file"
    , metavar "FILE"
    , help "A JSON file containing the unwitnessed transaction as a text envelope."
    ]

runBuildCommand :: BuildCommand -> IO ()
runBuildCommand BuildCommand{..} = do
  InAnyShelleyBasedEra era tbTxBody <- readTxBody bcTxBodyFile
  let bundle =
        TxBundle
          { tbGroupTab = U.empty
          , tbSigTab = U.empty
          , tbGroupMemTab = U.empty
          , tbTxBody
          }
  bundle' <- foldM processGroup bundle bcGroups
  case tbTxBody of
    TxBody TxBodyContent{txExtraKeyWits} -> case txExtraKeyWits of
      TxExtraKeyWitnessesNone ->
        hPutStrLn
          stderr
          "WARNING: The template transaction was assembled without extra key witnesses. The transaction may not validate if any are added."
      TxExtraKeyWitnesses _ keys -> unless (all (`U.elem` tbSigTab bundle') keys) do
        hPutStrLn
          stderr
          "WARNING: The template transaction was assembled without all extra key witnesses. Transaction fees and execution units may be higher than calculated."

  encodeFile bcOutFile $ SomeTxBundle era bundle'

processGroup :: TxBundle era -> Group -> IO (TxBundle era)
processGroup bundle Group{..}
  | fromIntegral groupThreshold > length groupMembers =
      crash id . vsep $
        [ "Group threshold too high."
        , "Threshold:" <+> pretty groupThreshold
        , "Group size:" <+> pretty (length groupMembers)
        ]
  | otherwise = do
      let (grpSize, bundle') = foldl' (uncurry processGroupMember) (0, bundle) groupMembers
      let grpThreshold = groupThreshold
      pure bundle'{tbGroupTab = U.snoc (tbGroupTab bundle) $ GroupHeader{..}}

processGroupMember
  :: Word16 -> TxBundle era -> Hash PaymentKey -> (Word16, TxBundle era)
processGroupMember count TxBundle{..} sig =
  ( succ count
  , TxBundle
      { tbGroupMemTab = U.snoc tbGroupMemTab $ GroupMemHeader{..}
      , tbSigTab = sigTab'
      , ..
      }
  )
  where
    gmGroup = fromIntegral $ U.length tbGroupTab
    (gmSig, sigTab') = case U.findIndex (== sig) tbSigTab of
      Nothing -> (fromIntegral $ U.length tbSigTab, U.snoc tbSigTab sig)
      Just ix -> (fromIntegral ix, tbSigTab)

-- * Info command

data InfoTarget
  = TargetBundle
  | TargetGroups
  | TargetGroup Int
  | TargetSignatory (Hash PaymentKey)
  | TargetTx ViewOutputFormat

data InfoCommand = InfoCommand
  { icTarget :: InfoTarget
  , icTxBundleFile :: FilePath
  }

infoCommandParser :: ParserInfo InfoCommand
infoCommandParser = info parser description
  where
    parser = InfoCommand <$> infoTargetParser <*> txBundleFileParser
    description = progDesc "Print transaction bundle information"

infoTargetParser :: Parser InfoTarget
infoTargetParser =
  asum
    [ flag' TargetGroups . fold $
        [ long "groups"
        , help "Print info about signatory groups"
        ]
    , flag'
        TargetTx
        ( fold
            [ long "tx"
            , help "Print info about the template transaction."
            ]
        )
        <*> asum
          [ flag' ViewOutputFormatJson . fold $
              [ long "json"
              , help "Render tx as JSON (default)"
              ]
          , flag' ViewOutputFormatYaml . fold $
              [ long "yaml"
              , help "Render tx as YAML"
              ]
          , pure ViewOutputFormatJson
          ]
    , fmap TargetGroup . option auto . fold $
        [ long "group"
        , help "Print info about a specific signatory group"
        ]
    , fmap TargetSignatory . hexOption (AsHash AsPaymentKey) . fold $
        [ long "signatory"
        , help "Print info about a specific signatory"
        ]
    , flag TargetBundle TargetBundle . fold $
        [ long "bundle"
        , help "Print top-level bundle information"
        ]
    ]

txBundleFileParser :: Parser FilePath
txBundleFileParser =
  strOption . fold $
    [ long "tx-bundle-file"
    , metavar "FILE"
    , help "A file containing a tx bundle."
    ]

runInfoCommand :: InfoCommand -> IO ()
runInfoCommand InfoCommand{..} = do
  SomeTxBundle era TxBundle{..} <- decodeFile icTxBundleFile
  case icTarget of
    TargetBundle -> do
      combos <- unwrapOrCrash renderGroupError $ signatureCombinations TxBundle{..}
      putDoc . vsep $
        [ "Transaction era:" <+> pretty era
        , "Signatory group count:" <+> pretty (U.length tbGroupTab)
        , "Signatory count:" <+> pretty (U.length tbSigTab)
        , "Total possible transaction count:" <+> pretty (Set.size combos)
        ]
    TargetGroups -> do
      putDoc $ vsep $ flip fmap [0 .. U.length tbGroupTab - 1] \i ->
        let GroupHeader{..} = tbGroupTab U.! i
         in hang 2 . vsep $
              [ "Group" <> brackets (pretty i) <> ":"
              , "Group size:" <+> pretty grpSize
              , "Group threshold:" <+> pretty grpThreshold
              ]
    TargetGroup i -> do
      GroupHeader{..} <- case tbGroupTab U.!? i of
        Nothing -> die "Invalid group index"
        Just tab -> pure tab
      signatories <- flip U.mapMaybeM tbGroupMemTab \GroupMemHeader{..} -> do
        if fromIntegral gmGroup == i
          then case tbSigTab U.!? fromIntegral gmSig of
            Nothing -> crash renderGroupError $ SignatoryIndexOutOufBounds gmSig
            Just sig -> pure $ Just sig
          else pure Nothing
      putDoc . vsep $
        [ "Group size:" <+> pretty grpSize
        , "Group threshold:" <+> pretty grpThreshold
        , "Group members:"
        , indent 2 $ vsep $ pretty . serialiseToRawBytesHexText <$> U.toList signatories
        ]
    TargetSignatory sig -> do
      i <- case U.findIndex (== sig) tbSigTab of
        Nothing -> die "Signatory not in any group"
        Just i -> pure i
      combos <- unwrapOrCrash renderGroupError $ signatureCombinations TxBundle{..}
      let groups = flip U.mapMaybe tbGroupMemTab \GroupMemHeader{..} -> do
            guard $ fromIntegral gmSig == i
            pure gmGroup
      putDoc . vsep $
        [ "Signatory index:" <+> pretty i
        , "Signatory groups:"
            <+> encloseSep emptyDoc emptyDoc comma (pretty <$> U.toList groups)
        , "Total possible transactions:"
            <+> pretty (Set.size $ Set.filter (Set.member sig) combos)
        ]
    TargetTx format -> do
      void $
        friendlyTxBody
          (viewOutputFormatToFriendlyFormat format)
          Nothing
          (toCardanoEra era)
          tbTxBody
  putStrLn ""

renderGroupError :: GroupError -> Doc AnsiStyle
renderGroupError = \case
  GroupIndexOutOufBounds group -> "Group index out of bounds: " <+> pretty group
  SignatoryIndexOutOufBounds sig -> "Signatory index out of bounds: " <+> pretty sig
  GroupSizeWrong expected actual ->
    vsep
      [ "Group size wrong"
      , "Expected:" <+> pretty expected
      , "Actual:" <+> pretty actual
      ]
  GroupThresholdTooHigh threshold size ->
    vsep
      [ "Group threshold too high."
      , "Threshold:" <+> pretty threshold
      , "Group size:" <+> pretty size
      ]

-- * Witness command

data WitnessCommand = WitnessCommand
  { wcSigningKeyFile :: FilePath
  , wcTxBundleFile :: FilePath
  , wcOutFile :: FilePath
  }

witnessCommandParser :: ParserInfo WitnessCommand
witnessCommandParser = info parser description
  where
    parser =
      WitnessCommand
        <$> signingKeyFileParser
        <*> txBundleFileParser
        <*> outFileParser "witness bundle"
    description = progDesc "Create a witness bundle file for a signing key"

signingKeyFileParser :: Parser FilePath
signingKeyFileParser =
  strOption . fold $
    [ long "signing-key-file"
    , metavar "FILE"
    , help "A file containing a signing key."
    ]

runWitnessCommand :: WitnessCommand -> IO ()
runWitnessCommand WitnessCommand{..} = do
  SomeTxBundle _ bundle <- decodeFile wcTxBundleFile
  sk <- readSigningKey wcSigningKeyFile
  witnessBundle <- unwrapOrCrash renderSignBundleError $ signBundle sk bundle
  encodeFile wcOutFile witnessBundle

renderSignBundleError :: SignBundleError -> Doc AnsiStyle
renderSignBundleError = \case
  SignGroupError ge -> renderGroupError ge
  SignBadEra -> "Unsupported transaction era"

-- * Witness Info command

data WitnessInfoTarget
  = WitTargetBundle
  | WitTargetTxs
  | WitTargetTx TxId

data WitnessInfoCommand = WitnessInfoCommand
  { wicTarget :: WitnessInfoTarget
  , wicBundleFile :: FilePath
  }

witnessInfoCommandParser :: ParserInfo WitnessInfoCommand
witnessInfoCommandParser = info parser description
  where
    parser = WitnessInfoCommand <$> witnessInfoTargetParser <*> witnessBundleFileParser
    description = progDesc "Print witness bundle information"

witnessInfoTargetParser :: Parser WitnessInfoTarget
witnessInfoTargetParser =
  asum
    [ flag' WitTargetTxs . fold $
        [ long "ls"
        , help "Print the IDs of the transactions witnessed by the bundle"
        ]
    , fmap WitTargetTx . hexOption AsTxId . fold $
        [ long "tx"
        , help "Print the signature for a specific transaction"
        ]
    , flag WitTargetBundle WitTargetBundle . fold $
        [ long "bundle"
        , help "Print top-level witness bundle information"
        ]
    ]

witnessBundleFileParser :: Parser FilePath
witnessBundleFileParser =
  strOption . fold $
    [ long "witness-bundle-file"
    , metavar "FILE"
    , help "A file containing a witness bundle."
    ]

runWitnessInfoCommand :: WitnessInfoCommand -> IO ()
runWitnessInfoCommand WitnessInfoCommand{..} = do
  WitnessBundle{..} <- decodeFile wicBundleFile
  case wicTarget of
    WitTargetBundle -> do
      putDoc . vsep $
        [ "Verification key:" <+> pretty (serialiseToRawBytesHexText wbVerificationKey)
        , "Signature count:" <+> pretty (Map.size wbSignatures)
        ]
      putStrLn ""
    WitTargetTx txId -> do
      case Map.lookup txId wbSignatures of
        Nothing -> die "Tx ID not found in witness bundle."
        Just (SigEd25519DSIGN bytes) ->
          T.putStr $ extractBase16 $ encodeBase16 $ psbToByteString bytes
    WitTargetTxs ->
      for_ (Map.keysSet wbSignatures) $ T.putStrLn . serialiseToRawBytesHexText

-- * Helpers

readTxBody :: FilePath -> IO (InAnyShelleyBasedEra TxBody)
readTxBody filePath = do
  result <-
    readFileTextEnvelopeAnyOf
      [ FromSomeType (AsTx AsAlonzoEra) $
          InAnyShelleyBasedEra ShelleyBasedEraAlonzo . getTxBody
      , FromSomeType (AsTx AsBabbageEra) $
          InAnyShelleyBasedEra ShelleyBasedEraBabbage . getTxBody
      , FromSomeType (AsTx AsConwayEra) $
          InAnyShelleyBasedEra ShelleyBasedEraConway . getTxBody
      ]
      (File filePath)
  unwrapOrCrash prettyError result

readSigningKey :: FilePath -> IO (SigningKey PaymentKey)
readSigningKey filePath = do
  result <- readFileTextEnvelope (AsSigningKey AsPaymentKey) (File filePath)
  unwrapOrCrash prettyError result

unwrapOrCrash :: (a -> Doc AnsiStyle) -> Either a b -> IO b
unwrapOrCrash renderer = either (crash renderer) pure

crash :: (a -> Doc AnsiStyle) -> a -> IO b
crash renderer err = do
  hPutDoc stderr $ renderer err
  exitFailure
