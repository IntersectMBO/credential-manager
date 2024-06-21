{-# LANGUAGE ApplicativeDo #-}

module Commands.InitHot (
  InitHotCommand (..),
  initHotCommandParser,
  runInitHotCommand,
) where

import Cardano.Api (
  AssetName,
  NetworkId,
  PolicyId,
  ScriptHash,
  StakeAddressReference (..),
  TxIn,
 )
import Commands.Common (
  StakeCredentialFile,
  assetNameParser,
  checkGroupSize,
  debugParser,
  networkIdParser,
  outDirParser,
  policyIdParser,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  runCommand,
  seedInputParser,
  stakeCredentialFileParser,
  votingCertParser,
  writeBech32ToFile,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import Control.Applicative (Alternative (..), asum, optional)
import CredentialManager.Orchestrator.InitHot
import Data.Foldable (Foldable (..), for_)
import Data.Traversable (for)
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  help,
  info,
  long,
  metavar,
  progDesc,
  strOption,
 )

data HotScriptInfoArgs = HotScriptInfoArgs
  { coldNFTPolicyId :: PolicyId
  , coldNFTAssetName :: AssetName
  , votingCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  }

data InitHotCommand = InitHotCommand
  { seedInput :: TxIn
  , networkId :: NetworkId
  , scriptInfoArgs :: Either ScriptHash HotScriptInfoArgs
  , debug :: Bool
  , outDir :: FilePath
  }

initHotCommandParser :: ParserInfo InitHotCommand
initHotCommandParser = info parser description
  where
    description :: InfoMod InitHotCommand
    description = progDesc "Initialize the hot CC credential scripts."

    parser :: Parser InitHotCommand
    parser =
      InitHotCommand
        <$> seedInputParser
        <*> networkIdParser
        <*> coldScriptArgsParser
        <*> debugParser
        <*> outDirParser

coldScriptArgsParser :: Parser (Either ScriptHash HotScriptInfoArgs)
coldScriptArgsParser =
  asum
    [ fmap Left $
        strOption $
          fold
            [ long "nft-lock-script-hash"
            , metavar "HEX"
            , help "The hash of script that will lock the NFT, as hex."
            ]
    , do
        coldNFTPolicyId <- policyIdParser coldNFTPolicyIdInfo
        coldNFTAssetName <- assetNameParser coldNFTAssetNameInfo
        votingCertFiles <- some votingCertParser
        stakeCredentialFile <- optional stakeCredentialFileParser
        pure $ Right HotScriptInfoArgs{..}
    ]

coldNFTPolicyIdInfo :: Mod OptionFields PolicyId
coldNFTPolicyIdInfo =
  fold
    [ long "cold-nft-policy-id"
    , help "The minting policy ID of the cold NFT."
    ]

coldNFTAssetNameInfo :: Mod OptionFields AssetName
coldNFTAssetNameInfo =
  fold
    [ long "cold-nft-token-name"
    , help "The token name of the cold NFT."
    ]

runInitHotCommand :: InitHotCommand -> IO ()
runInitHotCommand InitHotCommand{..} = do
  scriptInfo <- for scriptInfoArgs \HotScriptInfoArgs{..} -> do
    stakeAddress <-
      maybe
        (pure NoStakeAddress)
        readStakeAddressFile
        stakeCredentialFile
    votingUsers <- traverse readIdentityFromPEMFile' votingCertFiles
    checkGroupSize "voting" votingUsers
    pure HotScriptInfo{..}
  let inputs = InitHotInputs{..}
  InitHotOutputs{..} <- runCommand initHot inputs \case
    SeedTxIxTooLarge -> "The seed input has too large of an index. It must be less than 256."
    VotingTooSmall -> "At least 3 voting users are required"
    DuplicateVotingCertificates cert ->
      "Multiple voting users have the same certificate hash " <> show cert
    DuplicateVotingPubKeyHash key ->
      "Multiple voting users have the same public key hash " <> show key
  writeScriptToFile outDir "minting.plutus" mintingScript
  writeHexBytesToFile outDir "minting.plutus.hash" mintingScriptHash
  writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
  writeHexBytesToFile outDir "nft-token-name" hotNFTAssetName
  writeScriptToFile outDir "credential.plutus" credentialScript
  writeHexBytesToFile outDir "credential.plutus.hash" credentialScriptHash
  for_ nftScriptOutputs \HotNFTScriptOutputs{..} -> do
    writeScriptToFile outDir "nft.plutus" nftScript
    writeHexBytesToFile outDir "nft.plutus.hash" nftScriptHash
    writeBech32ToFile outDir "nft.addr" nftScriptAddress
    writePlutusDataToFile outDir "nft.datum.json" nftDatum
