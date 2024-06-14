module CredentialManager.Orchestrator.AuthorizeHot where

import Cardano.Api (
  Address,
  Certificate (..),
  ConwayEra,
  ConwayEraOnwards (..),
  PlutusScriptV3,
  Script,
  SerialiseAsRawBytes (..),
  ShelleyAddr,
  TxOut (..),
  UTxO,
  Value,
  txOutValueToValue,
 )
import Cardano.Api.Ledger (
  ConwayGovCert (..),
  ConwayTxCert (..),
  Credential (..),
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
import PlutusLedgerApi.V3 (
  Credential (..),
  HotCommitteeCredential (..),
  ScriptHash (..),
  toBuiltin,
 )

data AuthorizeHotInputs = AuthorizeHotInputs
  { coldCredentialScript :: Script PlutusScriptV3
  , hotCredentialScript :: Script PlutusScriptV3
  , scriptUtxo :: UTxO ConwayEra
  }
  deriving (Show, Eq, Generic)

data AuthorizeHotOutputs = AuthorizeHotOutputs
  { redeemer :: ColdLockRedeemer
  , outputDatum :: ColdLockDatum
  , outputAddress :: Address ShelleyAddr
  , outputValue :: Value
  , certificate :: Certificate ConwayEra
  }
  deriving (Show, Eq, Generic)

data AuthorizeHotError
  = AddressIsByron
  | AddressIsPayment
  | MissingDatum
  | NonInlineDatum
  | InvalidDatum
  | EmptyUTxO
  | AmbiguousUTxO
  deriving (Show, Eq, Generic)

authorizeHot
  :: AuthorizeHotInputs -> Either AuthorizeHotError AuthorizeHotOutputs
authorizeHot AuthorizeHotInputs{..} = do
  TxOut address inputValue txOutDatum _ <-
    extractOutput EmptyUTxO AmbiguousUTxO scriptUtxo
  outputAddress <- getScriptAddress AddressIsByron AddressIsPayment address
  inlineDatum <- getInlineDatum MissingDatum NonInlineDatum txOutDatum
  inputDatum <- decodeDatum InvalidDatum inlineDatum
  let coldCredentialScriptHash = hashScript coldCredentialScript
  let hotCredentialScriptHash = hashScript hotCredentialScript
  let hotCredentialScriptHashPlutus =
        ScriptHash $
          toBuiltin $
            serialiseToRawBytes hotCredentialScriptHash
  let hotCredential =
        HotCommitteeCredential $ ScriptCredential hotCredentialScriptHashPlutus
  let redeemer = AuthorizeHot hotCredential
  let outputDatum = inputDatum
  let outputValue = txOutValueToValue inputValue
  let certificate =
        ConwayCertificate ConwayEraOnwardsConway $
          ConwayTxCertGov $
            ConwayAuthCommitteeHotKey
              (ScriptHashObj $ toShelleyScriptHash coldCredentialScriptHash)
              (ScriptHashObj $ toShelleyScriptHash hotCredentialScriptHash)
  pure AuthorizeHotOutputs{..}
