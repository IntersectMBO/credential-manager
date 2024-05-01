module Commands.InitColdCredential (
  InitColdCredentialCommand (..),
  initColdCredentialCommandParser,
  runInitColdCredentialCommand,
) where

import Cardano.Api (PolicyId)
import Commands.Common (
  outDirParser,
  policyIdParser,
  runCommand,
  writeHexBytesToFile,
  writeScriptToFile,
 )
import CredentialManager.Orchestrator.InitColdCommittee
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

data InitColdCredentialCommand = InitColdCredentialCommand
  { policyId :: PolicyId
  , outDir :: FilePath
  }

initColdCredentialCommandParser :: ParserInfo InitColdCredentialCommand
initColdCredentialCommandParser = info parser description
  where
    description :: InfoMod InitColdCredentialCommand
    description =
      progDesc
        "Initialize the cold CC credential script by associating an NFT with it. The script will check that the NFT is spent whenever it needs to witness a transaction."

    parser :: Parser InitColdCredentialCommand
    parser =
      InitColdCredentialCommand
        <$> policyIdParser policyIdInfo
        <*> outDirParser

policyIdInfo :: Mod OptionFields PolicyId
policyIdInfo =
  fold
    [ long "policy-id"
    , short 'p'
    , help "The minting policy ID of the NFT to associate with the cold credential"
    ]

runInitColdCredentialCommand :: InitColdCredentialCommand -> IO ()
runInitColdCredentialCommand InitColdCredentialCommand{..} = do
  let inputs = InitColdCommitteeInputs policyId
  InitColdCommitteeOutputs{..} <- runCommand initColdCommittee inputs \case {}
  writeScriptToFile outDir "script.plutus" script
  writeHexBytesToFile outDir "script.hash" scriptHash
