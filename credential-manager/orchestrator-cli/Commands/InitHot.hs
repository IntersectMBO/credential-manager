module Commands.InitHot (
  InitHotCommand (..),
  initHotCommandParser,
  runInitHotCommand,
) where

import Cardano.Api (
  AssetName,
  NetworkId,
  PolicyId,
  StakeAddressReference (..),
  TxIn,
 )
import Commands.Common (
  StakeCredentialFile,
  assetNameParser,
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
import Control.Applicative (Alternative (..), optional)
import CredentialManager.Orchestrator.InitHot
import Data.Foldable (Foldable (..))
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  help,
  info,
  long,
  progDesc,
 )

data InitHotCommand = InitHotCommand
  { seedInput :: TxIn
  , networkId :: NetworkId
  , coldNFTPolicyId :: PolicyId
  , coldNFTAssetName :: AssetName
  , votingCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
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
        <*> policyIdParser coldNFTPolicyIdInfo
        <*> assetNameParser coldNFTAssetNameInfo
        <*> some votingCertParser
        <*> optional stakeCredentialFileParser
        <*> outDirParser

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
  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile
  votingUsers <- traverse readIdentityFromPEMFile' votingCertFiles
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
  writeScriptToFile outDir "nft.plutus" nftScript
  writeHexBytesToFile outDir "nft.plutus.hash" nftScriptHash
  writeBech32ToFile outDir "nft.addr" nftScriptAddress
  writePlutusDataToFile outDir "nft.datum.json" nftDatum
