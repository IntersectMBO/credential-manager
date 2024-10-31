module Main where

import Cardano.Api (
  AsType (..),
  ConsensusModeParams,
  ConwayEraOnwards (..),
  Doc,
  Error (..),
  Featured,
  File (..),
  FileDirection (..),
  FromSomeType (..),
  HasTypeProxy (..),
  Hash,
  Key (..),
  NetworkId,
  PaymentKey,
  ScriptValidity (..),
  SerialiseAsRawBytes,
  ShelleyBasedEra (..),
  ShelleyToBabbageEra,
  SlotNo,
  SocketPath,
  StakeAddress,
  ToCardanoEra (..),
  TxBody (..),
  TxBodyContent (..),
  TxExtraKeyWitnesses (..),
  TxId,
  TxIn,
  TxMetadataJsonSchema,
  TxValidityUpperBound,
  Value,
  WitCtxMint,
  WitCtxStake,
  WitCtxTxIn,
  castSigningKey,
  conwayEraOnwardsToShelleyBasedEra,
  deserialiseFromRawBytesHex,
  getTxBody,
  parseAddressAny,
  readFileTextEnvelope,
  readFileTextEnvelopeAnyOf,
  runExceptT,
  serialiseToRawBytesHexText,
  writeTxFileTextEnvelopeCddl,
 )
import Cardano.Api.Ledger (Coin)
import Cardano.CLI.Environment (EnvCli, getEnvCli)
import Cardano.CLI.EraBased.Commands.Transaction (TransactionBuildCmdArgs (..))
import Cardano.CLI.EraBased.Options.Common (
  pCertificateFile,
  pConsensusModeParams,
  pFeatured,
  pInvalidBefore,
  pInvalidHereafter,
  pMetadataFile,
  pMintMultiAsset,
  pNetworkId,
  pProposalFiles,
  pReadOnlyReferenceTxIn,
  pReturnCollateral,
  pScriptFor,
  pSocketPath,
  pTotalCollateral,
  pTreasuryDonation,
  pTxIn,
  pTxInCollateral,
  pTxMetadataJsonSchema,
  pTxOut,
  pUpdateProposalFile,
  pVoteFiles,
  pWithdrawal,
  pWitnessOverride,
  readerFromParsecParser,
 )
import Cardano.CLI.EraBased.Run.Transaction (runTransactionBuildCmd)
import Cardano.CLI.Json.Friendly (
  friendlyTxBody,
  viewOutputFormatToFriendlyFormat,
 )
import Cardano.CLI.Types.Common (
  BalanceTxExecUnits (..),
  CertificateFile,
  MetadataFile,
  ProposalFile,
  RequiredSigner (..),
  ScriptFile,
  ScriptWitnessFiles,
  TxBuildOutputOptions (..),
  TxOutAnyEra,
  TxOutChangeAddress (TxOutChangeAddress),
  TxOutShelleyBasedEra,
  TxTreasuryDonation,
  UpdateProposalFile,
  ViewOutputFormat (..),
 )
import Cardano.CLI.Types.Errors.TxCmdError (renderTxCmdError)
import Cardano.CLI.Types.Governance (VoteFile)
import Cardano.Crypto.DSIGN (SigDSIGN (..))
import Cardano.Crypto.PinnedSizedBytes (psbToByteString)
import Cardano.TxDynamic (
  AssembleError (..),
  GroupError (..),
  GroupHeader (..),
  GroupMemHeader (..),
  SignBundleError (..),
  SomeTxBundle (..),
  TxBundle (..),
  WitnessBundle (..),
  assemble,
  signBundle,
  signBundleAll,
  signatureCombinations,
  withShelleyBasedEra,
 )
import Control.Monad (foldM, guard, join, unless)
import Data.Base16.Types (extractBase16)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (decodeFile, encodeFile)
import Data.ByteString.Base16 (encodeBase16)
import Data.Data (Proxy (Proxy), Typeable)
import Data.Foldable (Foldable (..), asum, for_)
import Data.Functor (void)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.UUID.V4 (nextRandom)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Version (showVersion)
import Data.Word (Word8)
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
  optional,
  progDesc,
  short,
  strOption,
  switch,
 )
import Options.Applicative.Builder (command)
import Options.Applicative.Extra (hsubparser)
import Paths_tx_dynamic (version)
import Prettyprinter (
  Pretty (..),
  comma,
  emptyDoc,
  encloseSep,
  hang,
  indent,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (AnsiStyle, hPutDoc, putDoc)
import System.Directory (getTemporaryDirectory)
import System.Directory.Internal.Prelude (exitFailure, hPutStrLn)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (stderr)
import Text.Printf (printf)

main :: IO ()
main = do
  envCli <- getEnvCli
  join . execParser . info (helper <*> versionOption <*> rootParser envCli) . fold $
    [ fullDesc
    , progDesc
        "tx-bundle: a command line utility for interacting with transaction bundle files"
    ]

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("tx-dynamic " <> showVersion version)
    (long "version" <> short 'v' <> help "Show version.")

rootParser :: EnvCli -> Parser (IO ())
rootParser envCli =
  hsubparser $
    fold
      [ command "build" $
          runBuildCommand <$> buildCommandParser ConwayEraOnwardsConway envCli
      , command "witness" $ runWitnessCommand <$> witnessCommandParser
      , command "info" $ runInfoCommand <$> infoCommandParser
      , command "witness-info" $ runWitnessInfoCommand <$> witnessInfoCommandParser
      , command "assemble" $ runAssembleCommand <$> assembleCommandParser
      ]

-- * Build command

data Group = Group
  { groupName :: Text
  , groupThreshold :: Word8
  , groupMembers :: [Hash PaymentKey]
  }
  deriving (Show, Eq)

data BuildCommand era = BuildCommand
  { bcCurrentEra :: ConwayEraOnwards era
  , bcTxBodyFileOrBuildArgs :: Either FilePath (TransactionBuildArgs era)
  , bcGroups :: [Group]
  , bcOutFile :: FilePath
  }

data TransactionBuildArgs era = TransactionBuildArgs
  { nodeSocketPath :: !SocketPath
  , consensusModeParams :: !ConsensusModeParams
  , networkId :: !NetworkId
  , mScriptValidity :: !(Maybe ScriptValidity)
  -- ^ Mark script as expected to pass or fail validation
  , mOverrideWitnesses :: !(Maybe Word)
  -- ^ Override the required number of tx witnesses
  , txins :: ![(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ Transaction inputs with optional spending scripts
  , readOnlyReferenceInputs :: ![TxIn]
  -- ^ Read only reference inputs
  , txinsc :: ![TxIn]
  -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  , mReturnCollateral :: !(Maybe TxOutShelleyBasedEra)
  -- ^ Return collateral
  , mTotalCollateral :: !(Maybe Coin)
  -- ^ Total collateral
  , txouts :: ![TxOutAnyEra]
  -- ^ Normal outputs
  , changeAddresses :: !TxOutChangeAddress
  -- ^ A change output
  , mValue :: !(Maybe (Value, [ScriptWitnessFiles WitCtxMint]))
  -- ^ Multi-Asset value with script witness
  , mValidityLowerBound :: !(Maybe SlotNo)
  -- ^ Transaction validity lower bound
  , mValidityUpperBound :: !(TxValidityUpperBound era)
  -- ^ Transaction validity upper bound
  , certificates :: ![(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificates with potential script witness
  , withdrawals :: ![(StakeAddress, Coin, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Withdrawals with potential script witness
  , metadataSchema :: !TxMetadataJsonSchema
  , scriptFiles :: ![ScriptFile]
  -- ^ Auxiliary scripts
  , metadataFiles :: ![MetadataFile]
  , mUpdateProposalFile
      :: !(Maybe (Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)))
  , voteFiles :: ![(VoteFile 'In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , proposalFiles :: ![(ProposalFile 'In, Maybe (ScriptWitnessFiles WitCtxStake))]
  , treasuryDonation :: !(Maybe TxTreasuryDonation)
  }

buildCommandParser
  :: ConwayEraOnwards era -> EnvCli -> ParserInfo (BuildCommand era)
buildCommandParser era envCli = info parser description
  where
    parser =
      BuildCommand era
        <$> asum
          [ Left <$> txBodyFileParser
          , Right <$> buildArgsParser era envCli
          ]
        <*> groupsParser
        <*> outFileParser "transaction bundle"
    description = progDesc "Build a transaction bundle signable by groups of signatories"

buildArgsParser
  :: ConwayEraOnwards era -> EnvCli -> Parser (TransactionBuildArgs era)
buildArgsParser era envCli = do
  let sbe = conwayEraOnwardsToShelleyBasedEra era
  TransactionBuildArgs
    <$> pSocketPath envCli
    <*> pConsensusModeParams
    <*> pNetworkId envCli
    <*> optional pScriptValidity
    <*> optional pWitnessOverride
    <*> some (pTxIn sbe AutoBalance)
    <*> many pReadOnlyReferenceTxIn
    <*> many pTxInCollateral
    <*> optional pReturnCollateral
    <*> optional pTotalCollateral
    <*> many pTxOut
    <*> pChangeAddress
    <*> optional (pMintMultiAsset sbe AutoBalance)
    <*> optional pInvalidBefore
    <*> pInvalidHereafter sbe
    <*> many (pCertificateFile sbe AutoBalance)
    <*> many (pWithdrawal sbe AutoBalance)
    <*> pTxMetadataJsonSchema
    <*> many
      ( pScriptFor
          "auxiliary-script-file"
          Nothing
          "Filepath of auxiliary script(s)"
      )
    <*> many pMetadataFile
    <*> pFeatured (toCardanoEra sbe) (optional pUpdateProposalFile)
    <*> pVoteFiles sbe AutoBalance
    <*> pProposalFiles sbe AutoBalance
    <*> pTreasuryDonation sbe

pScriptValidity :: Parser ScriptValidity
pScriptValidity =
  asum
    [ flag' ScriptValid $
        mconcat
          [ long "script-valid"
          , help "Assertion that the script is valid. (default)"
          ]
    , flag' ScriptInvalid $
        mconcat
          [ long "script-invalid"
          , help $
              mconcat
                [ "Assertion that the script is invalid.  "
                , "If a transaction is submitted with such a script, "
                , "the script will fail and the collateral will be taken."
                ]
          ]
    ]

pChangeAddress :: Parser TxOutChangeAddress
pChangeAddress =
  fmap TxOutChangeAddress $
    option (readerFromParsecParser parseAddressAny) $
      mconcat
        [ long "change-address"
        , metavar "ADDRESS"
        , help "Address where ADA in excess of the tx fee will go to."
        ]

outFileParser :: String -> Parser FilePath
outFileParser assetType =
  strOption . fold $
    [ long "out-file"
    , metavar "FILE"
    , help $ printf "File to which to write the %s" $ show assetType
    ]

groupsParser :: Parser [Group]
groupsParser = nub <$> some groupParser

groupParser :: Parser Group
groupParser =
  Group
    <$> groupNameParser
    <*> groupThresholdParser
    <*> fmap nub (some groupMemberParser)

groupNameParser :: Parser Text
groupNameParser =
  strOption . fold $
    [ long "required-signer-group-name"
    , metavar "TEXT"
    , help
        "Starts a new group. The name of the group."
    ]

groupThresholdParser :: Parser Word8
groupThresholdParser =
  option auto . fold $
    [ long "required-signer-group-threshold"
    , metavar "INT"
    , help
        "The minimum number of signers from the group that must sign the transaction"
    ]

groupMemberParser :: Parser (Hash PaymentKey)
groupMemberParser =
  hexOption (AsHash AsPaymentKey) . fold $
    [ long "required-signer-hash"
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

runBuildCommand :: (Typeable era) => BuildCommand era -> IO ()
runBuildCommand BuildCommand{..} = do
  txBodyFile <-
    either pure (callTransactionBuild bcCurrentEra bcGroups) bcTxBodyFileOrBuildArgs
  tbTxBody <- readTxBody bcCurrentEra txBodyFile
  let bundle =
        TxBundle
          { tbGroupTab = U.empty
          , tbSigTab = U.empty
          , tbGroupMemTab = U.empty
          , tbGroupNameTab = V.empty
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

  encodeFile bcOutFile $
    SomeTxBundle (conwayEraOnwardsToShelleyBasedEra bcCurrentEra) bundle'

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
      pure
        bundle'
          { tbGroupTab = U.snoc (tbGroupTab bundle) $ GroupHeader{..}
          , tbGroupNameTab = V.snoc (tbGroupNameTab bundle) groupName
          }

processGroupMember
  :: Word8 -> TxBundle era -> Hash PaymentKey -> (Word8, TxBundle era)
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
  | TargetGroup Text
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
    , fmap TargetGroup . strOption . fold $
        [ long "group"
        , metavar "NAME"
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
              [ pretty (tbGroupNameTab V.! i) <> ":"
              , "Group size:" <+> pretty grpSize
              , "Group threshold:" <+> pretty grpThreshold
              ]
    TargetGroup name -> do
      (GroupHeader{..}, i) <- case V.findIndex (== name) tbGroupNameTab of
        Nothing -> die "Invalid group index"
        Just i -> pure (tbGroupTab U.! i, i)
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
  { wcAll :: Bool
  , wcSigningKeyFile :: FilePath
  , wcTxBundleFile :: FilePath
  , wcOutFile :: FilePath
  }

witnessCommandParser :: ParserInfo WitnessCommand
witnessCommandParser = info parser description
  where
    parser =
      WitnessCommand
        <$> allParser
        <*> signingKeyFileParser
        <*> txBundleFileParser
        <*> outFileParser "witness bundle"
    description = progDesc "Create a witness bundle file for a signing key"

allParser :: Parser Bool
allParser =
  switch . fold $
    [ long "all"
    , short 'a'
    , help
        "Sign all possible transactions even if not in a signing group. Useful for adding witnesses for non-script related purposes."
    ]

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
  witnessBundle <-
    unwrapOrCrash
      renderSignBundleError
      if wcAll
        then signBundleAll sk bundle
        else signBundle sk bundle
  encodeFile wcOutFile witnessBundle

renderSignBundleError :: SignBundleError -> Doc AnsiStyle
renderSignBundleError = \case
  SignGroupError ge -> renderGroupError ge
  SignBadEra -> "Unsupported transaction era"
  SignNoTransactions ->
    "Your signature is not required (hint: to forcibly sign all possible transaction combinations, for instance if you need to sign for non-script-related reasons, use the --all flag)."

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

-- * Assemble command

data AssembleCommand = AssembleCommand
  { acWitnessBundleFiles :: [FilePath]
  , acTxBundleFile :: FilePath
  , acOutFile :: FilePath
  }

assembleCommandParser :: ParserInfo AssembleCommand
assembleCommandParser = info parser description
  where
    parser =
      AssembleCommand
        <$> some witnessBundleFileParser
        <*> txBundleFileParser
        <*> outFileParser "signed transaction"
    description = progDesc "Create a signed transaction by combining several witness bundles."

runAssembleCommand :: AssembleCommand -> IO ()
runAssembleCommand AssembleCommand{..} = do
  SomeTxBundle era bundle <- decodeFile acTxBundleFile
  witnessBundles <- traverse decodeFile acWitnessBundleFiles
  tx <- unwrapOrCrash renderAssembleError $ assemble witnessBundles bundle
  unwrapOrCrash prettyError
    =<< writeTxFileTextEnvelopeCddl era (File acOutFile) tx

renderAssembleError :: AssembleError -> Doc AnsiStyle
renderAssembleError = \case
  AssembleGroupError ge -> renderGroupError ge
  AssembleBadEra -> "Unsupported transaction era"
  TooFewSignatures -> "Too few signatures"
  MissingSignature pkh txId ->
    vsep
      [ "Missing signature"
      , "TxId:" <+> pretty (serialiseToRawBytesHexText txId)
      , "Signatory:" <+> pretty (serialiseToRawBytesHexText pkh)
      ]

-- * Helpers

callTransactionBuild
  :: ConwayEraOnwards era -> [Group] -> TransactionBuildArgs era -> IO FilePath
callTransactionBuild ConwayEraOnwardsConway groups TransactionBuildArgs{..} = do
  tempDir <- getTemporaryDirectory
  uuid <- nextRandom
  let filename = printf "%s.txbody" $ show uuid
  let path = tempDir </> filename
  unwrapOrCrash renderTxCmdError =<< runExceptT do
    runTransactionBuildCmd
      TransactionBuildCmdArgs
        { eon = ShelleyBasedEraConway
        , requiredSigners = RequiredSignerHash <$> nub (groupMembers =<< groups)
        , buildOutputOptions = OutputTxBodyOnly $ File path
        , ..
        }
  pure path

readTxBody :: forall era. ConwayEraOnwards era -> FilePath -> IO (TxBody era)
readTxBody era filePath = withShelleyBasedEra (conwayEraOnwardsToShelleyBasedEra era) do
  result <-
    readFileTextEnvelope
      (AsTx $ proxyToAsType $ Proxy @era)
      (File filePath)
  getTxBody <$> unwrapOrCrash prettyError result

readSigningKey :: FilePath -> IO (SigningKey PaymentKey)
readSigningKey filePath = do
  result <-
    readFileTextEnvelopeAnyOf
      [ FromSomeType (AsSigningKey AsPaymentKey) id
      , FromSomeType (AsSigningKey AsGenesisUTxOKey) castSigningKey
      ]
      (File filePath)
  unwrapOrCrash prettyError result

unwrapOrCrash :: (a -> Doc AnsiStyle) -> Either a b -> IO b
unwrapOrCrash renderer = either (crash renderer) pure

crash :: (a -> Doc AnsiStyle) -> a -> IO b
crash renderer err = do
  hPutDoc stderr $ renderer err
  exitFailure
