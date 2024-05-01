module Commands.InitHotCredential (
  InitHotCredentialCommand (..),
  initHotCredentialCommandParser,
  runInitHotCredentialCommand,
) where

import Cardano.Api (PolicyId)
import Commands.Common (
  outDirParser,
  policyIdParser,
  runCommand,
  writeHexBytesToFile,
  writeScriptToFile,
 )
import CredentialManager.Orchestrator.InitHotCommittee
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
  short,
 )

data InitHotCredentialCommand = InitHotCredentialCommand
  { policyId :: PolicyId
  , outDir :: FilePath
  }

initHotCredentialCommandParser :: ParserInfo InitHotCredentialCommand
initHotCredentialCommandParser = info parser description
  where
    description :: InfoMod InitHotCredentialCommand
    description =
      progDesc
        "Initialize the hot CC credential script by associating an NFT with it. The script will check that the NFT is spent whenever it needs to witness a transaction."

    parser :: Parser InitHotCredentialCommand
    parser =
      InitHotCredentialCommand
        <$> policyIdParser policyIdInfo
        <*> outDirParser

policyIdInfo :: Mod OptionFields PolicyId
policyIdInfo =
  fold
    [ long "policy-id"
    , short 'p'
    , help "The minting policy ID of the NFT to associate with the hot credential"
    ]

runInitHotCredentialCommand :: InitHotCredentialCommand -> IO ()
runInitHotCredentialCommand InitHotCredentialCommand{..} = do
  let inputs = InitHotCommitteeInputs policyId
  InitHotCommitteeOutputs{..} <- runCommand initHotCommittee inputs \case {}
  writeScriptToFile outDir "script.plutus" script
  writeHexBytesToFile outDir "script.hash" scriptHash
