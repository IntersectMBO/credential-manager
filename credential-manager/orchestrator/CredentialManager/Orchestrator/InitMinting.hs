module CredentialManager.Orchestrator.InitMinting where

import Cardano.Api (
  AssetName (..),
  PlutusScriptV2,
  PlutusScriptVersion (..),
  Script,
  ScriptHash,
  SerialiseAsRawBytes (..),
  TxIn (..),
  TxIx (..),
 )
import Cardano.Api.Shelley (hashScript)
import Control.Monad (when)
import CredentialManager.Api (
  MintingRedeemer (..),
 )
import qualified CredentialManager.Debug.ScriptsV2 as DebugV2
import CredentialManager.Orchestrator.Common (serialiseScript)
import qualified CredentialManager.ScriptsV2 as ScriptsV2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (TxId (TxId), TxOutRef (..))
import qualified PlutusLedgerApi.V2 as PV2
import PlutusLedgerApi.V3 (
  toBuiltin,
 )

data DatumCheck
  = CheckCold
  | CheckHot
  deriving (Show, Eq, Generic)

data InitMintingInputs = InitMintingInputs
  { seedInput :: TxIn
  , destinationScript :: ScriptHash
  , datumCheck :: DatumCheck
  , debug :: Bool
  }
  deriving (Show, Eq, Generic)

data InitMintingOutputs = InitMintingOutputs
  { mintingScript :: Script PlutusScriptV2
  , mintingScriptHash :: ScriptHash
  , mintingRedeemer :: MintingRedeemer
  , assetName :: AssetName
  }
  deriving (Generic)

data InitMintingError
  = SeedTxIxTooLarge
  deriving (Show, Eq, Generic)

initMinting :: InitMintingInputs -> Either InitMintingError InitMintingOutputs
initMinting InitMintingInputs{..} = do
  let TxIn txId (TxIx txIx) = seedInput
  let txIdBytes = serialiseToRawBytes txId
  when (txIx >= 256) $ Left SeedTxIxTooLarge
  let txIxByte = fromIntegral txIx
  let tokenName = BS.drop 4 txIdBytes <> BS.pack [BS.c2w '#', txIxByte]
  let assetName = AssetName tokenName
  let mintingScript =
        serialiseScript
          PlutusScriptV2
          if debug
            then case datumCheck of
              CheckCold -> DebugV2.coldMinting
              CheckHot -> DebugV2.hotMinting
            else case datumCheck of
              CheckCold -> ScriptsV2.coldMinting
              CheckHot -> ScriptsV2.hotMinting
  let mintingScriptHash = hashScript mintingScript
  let txId' = TxId $ toBuiltin $ serialiseToRawBytes txId
  let txIx' = fromIntegral txIx
  let seedInput' = TxOutRef txId' txIx'
  let nftScriptHash' = PV2.ScriptHash $ toBuiltin $ serialiseToRawBytes destinationScript
  let mintingRedeemer = Mint seedInput' nftScriptHash'
  pure InitMintingOutputs{..}
