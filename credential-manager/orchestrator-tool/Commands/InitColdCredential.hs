module Commands.InitColdCredential (
  InitColdCredentialCommand (..),
  initColdCredentialCommandParser,
  runInitColdCredentialCommand,
) where

import Cardano.Api (
  PolicyId,
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
 )
import Commands.Common (
  outDirParser,
  policyIdParser,
  writeHexBytesToFile,
  writeScriptToFile,
 )
import qualified CredentialManager.Scripts as Scripts
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
import PlutusLedgerApi.V3 (CurrencySymbol (CurrencySymbol), toBuiltin)

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
  let compiledScript =
        Scripts.coldCommittee $
          CurrencySymbol $
            toBuiltin $
              serialiseToRawBytes policyId
  script <- writeScriptToFile outDir "script.plutus" compiledScript
  writeHexBytesToFile outDir "script.hash" $ hashScript script
