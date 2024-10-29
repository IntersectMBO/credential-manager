module Main where

import Cardano.Api (
  AsType (..),
  Doc,
  Error (prettyError),
  File (..),
  FromSomeType (FromSomeType),
  Hash,
  InAnyShelleyBasedEra (InAnyShelleyBasedEra),
  PaymentKey,
  SerialiseAsRawBytes,
  ShelleyBasedEra (..),
  TxBody,
  deserialiseFromRawBytesHex,
  getTxBody,
  readFileTextEnvelopeAnyOf,
 )
import Cardano.TxDynamic (
  GroupHeader (..),
  GroupMemHeader (..),
  SomeTxBundle (SomeTxBundle),
  TxBundle (..),
 )
import Control.Monad (foldM, join)
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (encodeFile)
import Data.Foldable (Foldable (..))
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import Prettyprinter (Pretty (..), vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, hPutDoc)
import System.Directory.Internal.Prelude (exitFailure)
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
  encodeFile bcOutFile . SomeTxBundle era =<< foldM processGroup bundle bcGroups

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

unwrapOrCrash :: (a -> Doc AnsiStyle) -> Either a b -> IO b
unwrapOrCrash renderer = either (crash renderer) pure

crash :: (a -> Doc AnsiStyle) -> a -> IO b
crash renderer err = do
  hPutDoc stderr $ renderer err
  exitFailure
