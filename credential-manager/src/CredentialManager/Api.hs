{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

module CredentialManager.Api (
  MintingRedeemer (..),
  CertificateHash (..),
  Identity (..),
  ColdLockDatum (..),
  ColdLockRedeemer (..),
  HotLockDatum (..),
  HotLockRedeemer (..),
  readIdentityFromPEMFile,
  parseIdentityFromPEMBytes,
  parsePrivateKeyFromPEMBytes,
) where

import Cardano.Api (
  AsType (..),
  Hash,
  Key (..),
  PaymentKey,
  SerialiseAsRawBytes (deserialiseFromRawBytes),
 )
import Cardano.Api.Byron (Hash (unPaymentKeyHash))
import Cardano.Api.Ledger (KeyHash (..), hashToBytes)
import Control.Monad (unless)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ASN1.BinaryEncoding (BER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types (ASN1Object (..))
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.PEM (PEM (..), pemParseBS)
import Data.String (IsString)
import Data.X509 (
  Certificate (..),
  PrivKey (..),
  PubKey (PubKeyEd25519),
  decodeSignedCertificate,
  getCertificate,
 )
import GHC.Generics (Generic)
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusTx.IsData as PlutusTx
import qualified PlutusTx.Lift as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.Show as PlutusTx

-- | Redeemer type for the minting validator
data MintingRedeemer
  = -- | Mint an NFT by consuming a tx out and sending it to a script address.
    Mint PV2.TxOutRef PV2.ScriptHash
  | -- | Burn a token by consuming it from a tx out.
    Burn PV2.TxOutRef
  deriving stock (Show, Eq, Generic)

-- | A SHA-256 hash of an X.509 certificate file.
newtype CertificateHash = CertificateHash {unCertificateHash :: PV3.BuiltinByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( PlutusTx.Eq
    , PlutusTx.Ord
    , PlutusTx.Show
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )
  deriving (IsString, Show) via PV3.LedgerBytes

-- | Represents a commitment to an X.509 certificate. Associates the hash of a
-- public key identified by the subject of a certificate with the hash of that
-- certificate.
data Identity = Identity
  { pubKeyHash :: PV3.PubKeyHash
  -- ^ The hash of a public key identified by the subject of an X.509 certificate.
  , certificateHash :: CertificateHash
  -- ^ The hash of the identifying X.509 certificate.
  }
  deriving stock (Show, Eq, Ord, Generic)

readIdentityFromPEMFile :: FilePath -> IO (Either String Identity)
readIdentityFromPEMFile = fmap parseIdentityFromPEMBytes . BS.readFile

parsePrivateKeyFromPEMBytes :: BS.ByteString -> Either String SecretKey
parsePrivateKeyFromPEMBytes pemBytes = do
  PEM{..} <-
    pemParseBS pemBytes >>= \case
      [] -> Left "Empty PEM file"
      [x] -> pure x
      _ -> Left "Unexpected number of PEM entries found"
  unless (pemName == "PRIVATE KEY") do
    Left $ "Unexpected PEM name: " <> pemName
  asn1 <- first show $ decodeASN1' BER pemContent
  pk <-
    first show (fromASN1 asn1) >>= \case
      (pk, []) -> pure pk
      _ -> Left "ASN1 stream end expected"
  case pk of
    PrivKeyEd25519 key -> pure key
    _ -> Left "Ed25519 private key expected"

parseIdentityFromPEMBytes :: BS.ByteString -> Either String Identity
parseIdentityFromPEMBytes pemBytes = do
  PEM{..} <-
    pemParseBS pemBytes >>= \case
      [] -> Left "Empty PEM file"
      [x] -> pure x
      _ -> Left "Unexpected number of PEM entries found"
  unless (pemName == "CERTIFICATE") do
    Left $ "Unexpected PEM name: " <> pemName
  Certificate{..} <- getCertificate <$> decodeSignedCertificate pemContent
  pubKey <- x509PubKeyToPaymentKey certPubKey
  let pubKeyHash = toPlutusPubKeyHash $ verificationKeyHash pubKey
  let certificateHash = hashCertificate pemBytes
  pure Identity{..}

hashCertificate :: BS.ByteString -> CertificateHash
hashCertificate =
  CertificateHash . PV3.toBuiltin . BS.pack . BA.unpack . hashWith SHA256

toPlutusPubKeyHash :: Hash PaymentKey -> PV3.PubKeyHash
toPlutusPubKeyHash =
  PV3.PubKeyHash . PV3.toBuiltin . hashToBytes . unKeyHash . unPaymentKeyHash
  where
    unKeyHash (KeyHash h) = h

x509PubKeyToPaymentKey :: PubKey -> Either String (VerificationKey PaymentKey)
x509PubKeyToPaymentKey (PubKeyEd25519 pk) =
  first show $
    deserialiseFromRawBytes (AsVerificationKey AsPaymentKey) $
      BS.pack $
        BA.unpack pk
x509PubKeyToPaymentKey _ = Left "Invalid Public Key Algorithm"

instance PlutusTx.Eq Identity where
  {-# INLINEABLE (==) #-}
  Identity pkh1 cert1 == Identity pkh2 cert2 =
    pkh1 PlutusTx.== pkh2 PlutusTx.&& cert1 PlutusTx.== cert2

instance PlutusTx.Ord Identity where
  {-# INLINEABLE compare #-}
  compare (Identity pkh1 cert1) (Identity pkh2 cert2) =
    case PlutusTx.compare pkh1 pkh2 of
      LT -> LT
      GT -> GT
      EQ -> PlutusTx.compare cert1 cert2

-- | The datum of the CC cold credential NFT locking script.
data ColdLockDatum = ColdLockDatum
  { certificateAuthority :: Identity
  -- ^ The identity of the certificate authority (CA) signing root of the X.509
  -- certificate chain. This has no on-chain effect; instead it serves as a
  -- public record of the CA which issued all other certificates in the chain,
  -- and can be used to verify the legitimacy of signatories on governance
  -- transactions.
  , membershipUsers :: [Identity]
  -- ^ A list of signatories who are authorized to administer the cold
  -- credential NFT, including changing the user lists (key rotation) and
  -- unlocking the NFT (for example to burn it or to send it to a new address).
  -- These users are also authorized to resign the cold credential from the CC,
  -- and to burn the NFT.
  , delegationUsers :: [Identity]
  -- ^ A list of signatories who are authorized to issue CC hot credential
  -- authorization certificates on behalf of the CC cold credential, and to
  -- rotate voting keys in the hot credential NFT.
  }
  deriving stock (Show, Eq, Ord, Generic)

instance PlutusTx.Eq ColdLockDatum where
  {-# INLINEABLE (==) #-}
  ColdLockDatum ca1 m1 d1 == ColdLockDatum ca2 m2 d2 =
    ca1 PlutusTx.== ca2 PlutusTx.&& m1 PlutusTx.== m2 PlutusTx.&& d1 PlutusTx.== d2

instance PlutusTx.Ord ColdLockDatum where
  {-# INLINEABLE compare #-}
  compare (ColdLockDatum ca1 m1 d1) (ColdLockDatum ca2 m2 d2) =
    case PlutusTx.compare ca1 ca2 of
      LT -> LT
      GT -> GT
      EQ -> case PlutusTx.compare m1 m2 of
        LT -> LT
        GT -> GT
        EQ -> PlutusTx.compare d1 d2

-- | The redeemer for the CC cold credential NFT locking script.
data ColdLockRedeemer
  = -- | Require the transaction to issue a hot credential authorization certificate
    -- on behalf of the cold credential. Transaction must be signed by a
    -- majority of identities from delegationUsers.
    --
    -- Forbids issuing any other certificates.
    -- Forbids burning the NFT.
    --
    -- Requires the address, value, and datum to be preserved between the input
    -- and output that contain the NFT.
    AuthorizeHot PV3.HotCommitteeCredential
  | -- | Require the transaction to issue a cold credential resignation certificate.
    -- Transaction must be signed by a majority of identities from membershipUsers.
    --
    -- Forbids issuing any other certificates.
    -- Forbids burning the NFT.
    --
    -- Requires the address, value, and datum to be preserved between the input
    -- and output that contain the NFT.
    ResignCold
  | -- | Requires the transaction to remove the requested identity from
    -- delegationUsers. Transaction must be signed by the resigning user.
    --
    -- Forbids issuing any certificates.
    -- Forbids burning the NFT.
    -- Forbids resigning the last delegation user.
    --
    -- Requires the address and value to be preserved between the input
    -- and output that contain the NFT.
    ResignDelegation Identity
  | -- | Requires the transaction to remove the requested identity from
    -- membershipUsers. Transaction must be signed by the resigning user.
    --
    -- Forbids issuing any certificates.
    -- Forbids burning the NFT.
    -- Forbids resigning the last membership user.
    --
    -- Requires the address and value to be preserved between the input
    -- and output that contain the NFT.
    ResignMembership Identity
  | -- | Allows the transaction to change the list of membership and / or
    -- delegation users. Transaction must be signed by a majority of identities
    -- from membershipUsers and by any users being added to one of the groups.
    --
    -- Forbids issuing any certificates.
    -- Forbids burning the NFT.
    -- Forbids empty membership groups.
    -- Forbids empty delegation groups.
    -- Requires signatures from the current (pre-rotation) membership group.
    -- Requires signatures from added membership keys.
    -- Requires signatures from added delegation keys.
    -- Requires the ca, address, and value to be preserved.
    RotateCold
  | -- | Allows the transaction to burn the NFT. Transaction must be
    -- signed by a majority of identities from membershipUsers.
    --
    -- Forbids issuing any certificates.
    -- Requires the NFT to be burned.
    BurnCold
  | -- | Allows the transaction to send the NFT to a new script address.
    -- Transaction must be signed by a majority of identities from
    -- membershipUsers.
    --
    -- Forbids issuing any certificates.
    -- Requires the NFT to be sent to the specified script.
    UpgradeCold PV3.ScriptHash
  deriving stock (Show, Eq, Generic)

instance PlutusTx.Eq ColdLockRedeemer where
  {-# INLINEABLE (==) #-}
  AuthorizeHot c1 == AuthorizeHot c2 = c1 PlutusTx.== c2
  AuthorizeHot _ == _ = PlutusTx.False
  ResignCold == ResignCold = PlutusTx.True
  ResignCold == _ = PlutusTx.False
  ResignDelegation d1 == ResignDelegation d2 = d1 PlutusTx.== d2
  ResignDelegation _ == _ = PlutusTx.False
  ResignMembership m1 == ResignMembership m2 = m1 PlutusTx.== m2
  ResignMembership _ == _ = PlutusTx.False
  RotateCold == RotateCold = PlutusTx.True
  RotateCold == _ = PlutusTx.False
  BurnCold == BurnCold = PlutusTx.True
  BurnCold == _ = PlutusTx.False
  UpgradeCold h1 == UpgradeCold h2 = h1 PlutusTx.== h2
  UpgradeCold _ == _ = PlutusTx.False

PlutusTx.makeLift ''CertificateHash

PlutusTx.deriveShow ''Identity
PlutusTx.makeLift ''Identity
PlutusTx.makeIsDataIndexed ''Identity [('Identity, 0)]

PlutusTx.deriveShow ''ColdLockDatum
PlutusTx.makeLift ''ColdLockDatum
PlutusTx.makeIsDataIndexed ''ColdLockDatum [('ColdLockDatum, 0)]

PlutusTx.makeLift ''ColdLockRedeemer
PlutusTx.makeIsDataIndexed
  ''ColdLockRedeemer
  [ ('AuthorizeHot, 0)
  , ('ResignCold, 1)
  , ('ResignDelegation, 2)
  , ('ResignMembership, 3)
  , ('RotateCold, 4)
  , ('BurnCold, 5)
  , ('UpgradeCold, 6)
  ]

-- | The datum of the CC hot credential NFT locking script.
newtype HotLockDatum = HotLockDatum
  { votingUsers :: [Identity]
  -- ^ A list of signatories who are authorized to cast committee votes.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype
    ( PlutusTx.Eq
    , PlutusTx.Ord
    , PlutusTx.Show
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

-- | The redeemer for the CC hot credential NFT locking script.
data HotLockRedeemer
  = -- | Require the transaction to cast votes only for this hot credential.
    -- Transaction must be signed by a majority of identities from votingUsers.
    --
    -- Forbids casting any other votes that are not from this hot credential.
    -- Forbids burning the NFT.
    --
    -- Requires the address, value, and datum to be preserved between the input
    -- and output that contain the NFT.
    Vote
  | -- | Requires the transaction to remove the requested identity from
    -- votingUsers. Transaction must be signed by the resigning user.
    --
    -- Forbids casting any votes.
    -- Forbids burning the NFT.
    -- Forbids resigning the last voting user.
    --
    -- Requires the address and value to be preserved between the input
    -- and output that contain the NFT.
    ResignVoting Identity
  | -- | Allows the transaction to change the list of voting users. Transaction
    -- must be signed by a majority of identities from delegationUsers.
    --
    -- In order to obtain the list of delegation users, the transaction must
    -- reference the output containing the cold credential NFT as a reference
    -- input.
    --
    -- Forbids casting any votes.
    -- Forbids burning the NFT.
    -- Forbids empty voting groups.
    -- Requires signatures from the input delegation group
    -- Requires signatures from added voting keys
    -- Requires the address and value to be preserved.
    RotateHot
  | -- | Allows the transaction to burn the NFT. Transaction must be signed by
    -- a majority of identities from membershipUsers.
    --
    -- In order to obtain the list of delegation users, the transaction must
    -- reference the output containing the cold credential NFT as a reference
    -- input.
    --
    -- Forbids casting any votes.
    -- Requires the NFT to be sent to the specified script.
    BurnHot
  | -- | Allows the transaction to send the NFT to a new script address.
    -- Transaction must be signed by a majority of identities from
    -- membershipUsers.
    --
    -- In order to obtain the list of delegation users, the transaction must
    -- reference the output containing the cold credential NFT as a reference
    -- input.
    --
    -- Forbids casting any votes.
    -- Requires the NFT to be sent to the specified script.
    UpgradeHot PV3.ScriptHash
  deriving stock (Show, Eq, Generic)

instance PlutusTx.Eq HotLockRedeemer where
  {-# INLINEABLE (==) #-}
  Vote == Vote = PlutusTx.True
  Vote == _ = PlutusTx.False
  ResignVoting v1 == ResignVoting v2 = v1 PlutusTx.== v2
  ResignVoting _ == _ = PlutusTx.False
  RotateHot == RotateHot = PlutusTx.True
  RotateHot == _ = PlutusTx.False
  BurnHot == BurnHot = PlutusTx.True
  BurnHot == _ = PlutusTx.False
  UpgradeHot h1 == UpgradeHot h2 = h1 PlutusTx.== h2
  UpgradeHot _ == _ = PlutusTx.False

PlutusTx.makeLift ''HotLockDatum
PlutusTx.makeLift ''HotLockRedeemer

-- Using non-zero based indexes to easily differentiate cold and hot lock
-- redeemers.
PlutusTx.makeIsDataIndexed
  ''HotLockRedeemer
  [ ('Vote, 7)
  , ('ResignVoting, 8)
  , ('RotateHot, 9)
  , ('BurnHot, 10)
  , ('UpgradeHot, 11)
  ]

PlutusTx.makeIsDataIndexed
  ''MintingRedeemer
  [ ('Mint, 0)
  , ('Burn, 1)
  ]
