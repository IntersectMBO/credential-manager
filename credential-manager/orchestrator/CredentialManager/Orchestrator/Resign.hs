module CredentialManager.Orchestrator.Resign where

import Cardano.Api (
  Address,
  Certificate (..),
  ConwayEra,
  ConwayEraOnwards (..),
  PlutusScriptV3,
  Script,
  ShelleyAddr,
  TxOut (..),
  UTxO,
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
  extractOutput,
  getInlineDatum,
  getScriptAddress,
 )
import GHC.Generics (Generic)

data ResignInputs = ResignInputs
  { coldCredentialScript :: Script PlutusScriptV3
  , metadataUrl :: Url
  , metadataHash :: SafeHash StandardCrypto AnchorData
  , scriptUtxo :: UTxO ConwayEra
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
  | EmptyUTxO
  | AmbiguousUTxO
  deriving (Show, Eq, Generic)

resign :: ResignInputs -> Either ResignError ResignOutputs
resign ResignInputs{..} = do
  TxOut address inputValue txOutDatum _ <-
    extractOutput EmptyUTxO AmbiguousUTxO scriptUtxo
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
