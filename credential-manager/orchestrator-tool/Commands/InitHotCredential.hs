module Commands.InitHotCredential where

import Cardano.Api (hashScript)
import Commands.InitColdCredential (
  policyIdParser,
  scriptHashOutParser,
  scriptOutParser,
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
import PlutusLedgerApi.V3 (CurrencySymbol)

data InitHotCredentialCommand = InitHotCredentialCommand
  { policyId :: CurrencySymbol
  , scriptOut :: FilePath
  , scriptHashOut :: FilePath
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
        <*> scriptOutParser
        <*> scriptHashOutParser

policyIdInfo :: Mod OptionFields CurrencySymbol
policyIdInfo =
  fold
    [ long "policy-id"
    , short 'p'
    , help "The minting policy ID of the NFT to associate with the hot credential"
    ]

runInitHotCredentialCommand :: InitHotCredentialCommand -> IO ()
runInitHotCredentialCommand InitHotCredentialCommand{..} = do
  let compiledScript = Scripts.hotCommittee policyId
  script <- writeScriptToFile scriptOut compiledScript
  writeHexBytesToFile scriptHashOut $ hashScript script
