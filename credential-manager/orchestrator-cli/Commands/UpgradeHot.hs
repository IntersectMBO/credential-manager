module Commands.UpgradeHot (
  UpgradeHotCommand (..),
  upgradeHotCommandParser,
  runUpgradeHotCommand,
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
  HotLockRedeemer (..),
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

data UpgradeHotCommand = UpgradeHotCommand
  { outDir :: FilePath
  , newScript :: Either ScriptHash FilePath
  }

upgradeHotCommandParser :: ParserInfo UpgradeHotCommand
upgradeHotCommandParser = info parser description
  where
    description :: InfoMod UpgradeHotCommand
    description =
      progDesc
        "Upgrade the hot NFT lock script by sending it to a new script address. WARNING: the datum in the new script output is not checked."

    parser :: Parser UpgradeHotCommand
    parser = UpgradeHotCommand <$> outDirParser <*> newScriptParser

runUpgradeHotCommand :: UpgradeHotCommand -> IO ()
runUpgradeHotCommand UpgradeHotCommand{..} = do
  scriptHash <- resolveScriptHash newScript
  writePlutusDataToFile outDir "redeemer.json" $ UpgradeHot scriptHash

resolveScriptHash :: Either ScriptHash FilePath -> IO PV3.ScriptHash
resolveScriptHash =
  either
    (pure . PV3.ScriptHash . toBuiltin . serialiseToRawBytes)
    (resolveScriptHash . Left . hashScript <=< readFilePlutusV3Script)
