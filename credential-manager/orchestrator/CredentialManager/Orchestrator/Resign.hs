module CredentialManager.Orchestrator.Resign where

import Cardano.Api (
  Address,
  Certificate (..),
  ConwayEra,
  ConwayEraOnwards (..),
  CtxUTxO,
  PlutusScriptV3,
  Script,
  ShelleyAddr,
  TxOut (..),
  Value,
  txOutValueToValue,
 )
import Cardano.Api.Ledger (
  Anchor (..),
  AnchorData,
  ConwayGovCert (..),
  ConwayTxCert (..),
  Credential (..),
  SafeHash,
  StandardCrypto,
  StrictMaybe (..),
  Url,
 )
import Cardano.Api.Shelley (hashScript, toShelleyScriptHash)
import CredentialManager.Api
import CredentialManager.Orchestrator.Common (
  decodeDatum,
  getInlineDatum,
  getScriptAddress,
 )
import GHC.Generics (Generic)

data ResignInputs = ResignInputs
  { coldCredentialScript :: Script PlutusScriptV3
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
  , scriptUtxo :: TxOut CtxUTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data ResignOutputs = ResignOutputs
  { redeemer :: ColdLockRedeemer
  , outputDatum :: ColdLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  , certificate :: Certificate ConwayEra
  }
  deriving (Show, Eq, Generic)

data ResignError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  deriving (Show, Eq, Generic)

resign :: ResignInputs -> Either ResignError ResignOutputs
resign ResignInputs{..} = do
  let TxOut address inputValue txOutDatum _ = scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let coldCredentialScriptHash = hashScript coldCredentialScript
  let coldCredential =
        ScriptHashObj $ toShelleyScriptHash coldCredentialScriptHash
  let redeemer = ResignCold
  let outputDatum = inputDatum
  let outputValue = txOutValueToValue inputValue
  let certificate =
        ConwayCertificate ConwayEraOnwardsConway
          . ConwayTxCertGov
          . ConwayResignCommitteeColdKey coldCredential
          . SJust
          $ Anchor metadataUrl metadataHash
  pure ResignOutputs{..}
