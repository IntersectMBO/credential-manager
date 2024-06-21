module Commands.InitCold (
  InitColdCommand (..),
  initColdCommandParser,
  runInitColdCommand,
) where

import Cardano.Api (AssetName, NetworkId, PolicyId, StakeAddressReference (..))
import Commands.Common (
  StakeCredentialFile,
  assetNameParser,
  checkGroupSize,
  debugParser,
  delegationCertParser,
  membershipCertParser,
  networkIdParser,
  outDirParser,
  policyIdParser,
  readIdentityFromPEMFile',
  readStakeAddressFile,
  runCommand,
  seedInputParser,
  stakeCredentialFileParser,
  writeBech32ToFile,
  writeHexBytesToFile,
  writePlutusDataToFile,
  writeScriptToFile,
 )
import Control.Applicative (Alternative (..), asum, optional)
import CredentialManager.Orchestrator.InitCold
import CredentialManager.Orchestrator.InitMinting (InitMintingOutputs (..))
import Data.Foldable (Foldable (..), for_)
import Options.Applicative (
  InfoMod,
  Mod,
  OptionFields,
  Parser,
  ParserInfo,
  action,
  help,
  info,
  long,
  metavar,
  progDesc,
  strOption,
 )

data InitColdCommand = InitColdCommand
  { nftInfo :: NFTInfo
  , networkId :: NetworkId
  , caCertFile :: FilePath
  , membershipCertFiles :: [FilePath]
  , delegationCertFiles :: [FilePath]
  , stakeCredentialFile :: Maybe StakeCredentialFile
  , debug :: Bool
  , outDir :: FilePath
  }

initColdCommandParser :: ParserInfo InitColdCommand
initColdCommandParser = info parser description
  where
    description :: InfoMod InitColdCommand
    description = progDesc "Initialize the cold CC credential scripts."

    parser :: Parser InitColdCommand
    parser =
      InitColdCommand
        <$> nftInfoParser
        <*> networkIdParser
        <*> caCertFileParser
        <*> some membershipCertParser
        <*> some delegationCertParser
        <*> optional stakeCredentialFileParser
        <*> debugParser
        <*> outDirParser

nftInfoParser :: Parser NFTInfo
nftInfoParser =
  asum
    [ NFTInfoMint <$> seedInputParser
    , NFTInfoRaw
        <$> policyIdParser policyIdInfo
        <*> assetNameParser assetNameInfo
    ]

policyIdInfo :: Mod OptionFields PolicyId
policyIdInfo =
  fold
    [ long "policy-id"
    , help "The minting policy ID of the NFT to use."
    ]

assetNameInfo :: Mod OptionFields AssetName
assetNameInfo =
  fold
    [ long "token-name"
    , help "The token name of the NFT to use."
    ]

caCertFileParser :: Parser FilePath
caCertFileParser =
  strOption $
    fold
      [ long "ca-cert"
      , metavar "FILE_PATH"
      , help "A relative path to the root CA certificate PEM file."
      , action "file"
      ]

runInitColdCommand :: InitColdCommand -> IO ()
runInitColdCommand InitColdCommand{..} = do
  stakeAddress <-
    maybe
      (pure NoStakeAddress)
      readStakeAddressFile
      stakeCredentialFile
  certificateAuthority <- readIdentityFromPEMFile' caCertFile
  membershipUsers <- traverse readIdentityFromPEMFile' membershipCertFiles
  delegationUsers <- traverse readIdentityFromPEMFile' delegationCertFiles
  checkGroupSize "membership" membershipUsers
  checkGroupSize "delegation" delegationUsers
  let inputs = InitColdInputs{..}
  InitColdOutputs{..} <- runCommand initCold inputs \case
    SeedTxIxTooLarge -> "The seed input has too large of an index. It must be less than 256."
    NonMintOnMainnet -> "Refusing to use arbitrary NFT on mainnet."
    MembershipTooSmall -> "At least 3 membership users are required"
    DuplicateMembershipCertificates cert ->
      "Multiple membership users have the same certificate hash " <> show cert
    DuplicateMembershipPubKeyHash key ->
      "Multiple membership users have the same public key hash " <> show key
    DelegationTooSmall -> "At least 3 delegation users are required"
    DuplicateDelegationCertificates cert ->
      "Multiple delegation users have the same certificate hash " <> show cert
    DuplicateDelegationPubKeyHash key ->
      "Multiple delegation users have the same public key hash " <> show key
  for_ mintingOutputs \InitMintingOutputs{..} -> do
    writeScriptToFile outDir "minting.plutus" mintingScript
    writeHexBytesToFile outDir "minting.plutus.hash" mintingScriptHash
    writePlutusDataToFile outDir "mint.redeemer.json" mintingRedeemer
    writeHexBytesToFile outDir "nft-token-name" assetName
  writeScriptToFile outDir "credential.plutus" credentialScript
  writeHexBytesToFile outDir "credential.plutus.hash" credentialScriptHash
  writeScriptToFile outDir "nft.plutus" nftScript
  writeHexBytesToFile outDir "nft.plutus.hash" nftScriptHash
  writeBech32ToFile outDir "nft.addr" nftScriptAddress
  writePlutusDataToFile outDir "nft.datum.json" nftDatum
