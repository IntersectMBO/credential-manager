module Commands.UpgradeCold (
  UpgradeColdCommand (..),
  upgradeColdCommandParser,
  runUpgradeColdCommand,
) where

import Cardano.Api (
  ScriptHash,
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
 )
import Commands.Common (
  newScriptParser,
  outDirParser,
  readFilePlutusV3Script,
  writePlutusDataToFile,
 )
import Control.Monad ((<=<))
import CredentialManager.Api (
  ColdLockRedeemer (..),
 )
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  info,
  progDesc,
 )
import PlutusLedgerApi.V3 (toBuiltin)
import qualified PlutusLedgerApi.V3 as PV3

data UpgradeColdCommand = UpgradeColdCommand
  { outDir :: FilePath
  , newScript :: Either ScriptHash FilePath
  }

upgradeColdCommandParser :: ParserInfo UpgradeColdCommand
upgradeColdCommandParser = info parser description
  where
    description :: InfoMod UpgradeColdCommand
    description =
      progDesc
        "Upgrade the cold NFT lock script by sending it to a new script address. WARNING: the datum in the new script output is not checked."

    parser :: Parser UpgradeColdCommand
    parser = UpgradeColdCommand <$> outDirParser <*> newScriptParser

runUpgradeColdCommand :: UpgradeColdCommand -> IO ()
runUpgradeColdCommand UpgradeColdCommand{..} = do
  scriptHash <- resolveScriptHash newScript
  writePlutusDataToFile outDir "redeemer.json" $ UpgradeCold scriptHash

resolveScriptHash :: Either ScriptHash FilePath -> IO PV3.ScriptHash
resolveScriptHash =
  either
    (pure . PV3.ScriptHash . toBuiltin . serialiseToRawBytes)
    (resolveScriptHash . Left . hashScript <=< readFilePlutusV3Script)
