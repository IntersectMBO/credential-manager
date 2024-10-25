{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TxDynamic where

import Cardano.Api (
  AddressInEra (AddressInEra),
  AddressTypeInEra (ShelleyAddressInEra),
  AnyShelleyBasedEra (..),
  AsType (..),
  ConwayEra,
  CtxUTxO,
  EpochNo (..),
  HasTypeProxy (proxyToAsType),
  Hash,
  IsShelleyBasedEra,
  NetworkId (Mainnet),
  PaymentCredential (..),
  PaymentKey,
  SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR),
  ShelleyBasedEra (..),
  SlotNo (..),
  StakeAddressReference (NoStakeAddress),
  SystemStart (..),
  TxBody,
  TxId (..),
  TxIn (..),
  TxIx (..),
  TxOut (..),
  TxOutDatum (TxOutDatumNone),
  UTxO (..),
  lovelaceToTxOutValue,
  makeShelleyAddress,
  readFileTextEnvelope,
 )
import qualified Cardano.Api as C
import Cardano.Api.Ledger (KeyHash (..), StandardCrypto)
import Cardano.Api.Shelley (
  Hash (..),
  LedgerProtocolParameters (..),
  ReferenceScript (..),
  fromShelleyTxOut,
  toShelleyTxOut,
 )
import Cardano.Binary (DecoderError, decodeFull', serialize')
import Cardano.Crypto.Hash (
  PackedBytes (..),
  hashFromPackedBytes,
  hashToBytesShort,
  hashToPackedBytes,
 )
import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson (decodeFileStrict')
import Data.Array.Byte (ByteArray (ByteArray))
import Data.Binary (
  Binary (..),
  Get,
  getWord8,
  putWord8,
 )
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString (..))
import Data.Coerce (coerce)
import Data.Data (Proxy (..), Typeable)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Reflection (give)
import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Time (
  UTCTime (UTCTime),
  nominalDiffTimeToSeconds,
  secondsToDiffTime,
  secondsToNominalDiffTime,
 )
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word16, Word32, Word64)
import Data.Word.Extra (indexWord32BE, indexWord64BE)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block (
  EpochSize (EpochSize),
  GenesisWindow (GenesisWindow),
 )
import Ouroboros.Consensus.BlockchainTime (
  RelativeTime (RelativeTime),
  mkSlotLength,
 )
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (EraParams, eraEpochSize, eraGenesisWin, eraSafeZone, eraSlotLength),
  EraParamsFormat (..),
  EraSummary (..),
  SafeZone (UnsafeIndefiniteSafeZone),
  Summary (..),
 )
import System.IO.Unsafe (unsafePerformIO)

-- | Header in a tx bundle file
data TxBundleHeader = TxBundleHeader
  { hEra :: AnyShelleyBasedEra
  -- ^ The era of the transaction
  , hGroupTabSize :: Word16
  -- ^ The length of the group table (in rows)
  , hSigTabSize :: Word16
  -- ^ The length of the signatory table (in rows)
  , hGroupMemTabSize :: Word16
  -- ^ The length of the group membership table (in rows)
  , hUtxoTabSize :: Word16
  -- ^ The length of the utxo table (in rows)
  , hSummarySize :: Int
  -- ^ The size of the Summary (in bytes)
  , hPParamsSize :: Int
  -- ^ The size of the protocol parameters (in bytes)
  , hTxBodySize :: Int
  -- ^ The size of the tx body (in bytes)
  }
  deriving (Show, Eq, Generic, Binary)

-- | A row in the group table
data GroupHeader = GroupHeader
  { grpSize :: Word16
  -- ^ The number of signatories in the group
  , grpThreshold :: Word16
  -- ^ The minimum number of signatures required from the group
  }
  deriving (Show, Eq, Generic, Binary)

-- | A row in the group membership table
data GroupMemHeader = GroupMemHeader
  { gmGroup :: Word16
  -- ^ The group to which the signatory belongs
  , gmSig :: Word16
  -- ^ The signatory that belongs to the group
  }
  deriving (Show, Eq, Generic, Binary)

-- | A row in the UTxO table
data UtxoHeader = UtxoHeader
  { utxoTxId :: TxId
  -- ^ The ID of th transaction that produced the output
  , utxoTxIx :: TxIx
  -- ^ The index of the output within the producing transaction
  , utxoTxOutSize :: Int
  -- ^ The size of the tx out (in bytes)
  }
  deriving (Show, Eq, Generic, Binary)

data TxBundle era = TxBundle
  { tbGroupTab :: Vector GroupHeader
  , tbSigTab :: Vector (Hash PaymentKey)
  , tbGroupMemTab :: Vector GroupMemHeader
  , tbSystemStart :: SystemStart
  , tbSummary :: Summary (CardanoEras StandardCrypto)
  , tbPParams :: LedgerProtocolParameters era
  , tbUtxo :: UTxO era
  , tbTxBody :: TxBody era
  }
  deriving (Show, Eq)

testBundle :: TxBundle ConwayEra
testBundle =
  TxBundle
    { tbGroupTab =
        U.fromList
          [ GroupHeader 1 1
          , GroupHeader 3 2
          , GroupHeader 1 1
          ]
    , tbSigTab =
        U.fromList
          [ PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 0 0 0 0
          , PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 0 0 0 1
          , PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 0 0 0 2
          , PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 0 0 0 4
          ]
    , tbGroupMemTab =
        U.fromList
          [ GroupMemHeader 0 0
          , GroupMemHeader 1 1
          , GroupMemHeader 1 2
          , GroupMemHeader 1 3
          , GroupMemHeader 2 2
          ]
    , tbSystemStart =
        SystemStart $ UTCTime (fromOrdinalDate 2024 0) $ secondsToDiffTime 0
    , tbSummary =
        Summary
          . NonEmptyCons eraSummary
          . NonEmptyCons eraSummary
          . NonEmptyCons eraSummary
          . NonEmptyCons eraSummary
          . NonEmptyCons eraSummary
          . NonEmptyCons eraSummary
          $ NonEmptyOne eraSummary
    , tbPParams =
        unsafePerformIO $
          LedgerProtocolParameters . fromJust <$> decodeFileStrict' "../pparams.json"
    , tbUtxo =
        UTxO . Map.fromList $
          [
            ( TxIn (TxId $ hashFromPackedBytes $ PackedBytes32 0 0 0 0) (TxIx 0)
            , TxOut
                addr1
                (lovelaceToTxOutValue ShelleyBasedEraConway 10)
                TxOutDatumNone
                ReferenceScriptNone
            )
          ,
            ( TxIn (TxId $ hashFromPackedBytes $ PackedBytes32 0 0 0 0) (TxIx 1)
            , TxOut
                addr1
                (lovelaceToTxOutValue ShelleyBasedEraConway 20)
                TxOutDatumNone
                ReferenceScriptNone
            )
          ]
    , tbTxBody =
        either (error . show) C.getTxBody . unsafePerformIO $
          readFileTextEnvelope (AsTx AsConwayEra) "../tx.json"
    }

addr1 :: AddressInEra ConwayEra
addr1 =
  AddressInEra (ShelleyAddressInEra ShelleyBasedEraConway) $
    makeShelleyAddress
      Mainnet
      ( PaymentCredentialByKey $
          PaymentKeyHash $
            KeyHash $
              hashFromPackedBytes $
                PackedBytes28 0 0 0 0
      )
      NoStakeAddress

eraSummary :: EraSummary
eraSummary =
  EraSummary
    { eraStart =
        Bound
          { boundTime = RelativeTime 0
          , boundSlot = SlotNo 0
          , boundEpoch = EpochNo 0
          }
    , eraEnd = EraUnbounded
    , eraParams =
        EraParams
          { eraEpochSize = EpochSize 0
          , eraSlotLength = mkSlotLength 1
          , eraSafeZone = UnsafeIndefiniteSafeZone
          , eraGenesisWin = GenesisWindow 0
          }
    }

data SomeTxBundle where
  SomeTxBundle
    :: (Typeable era) => ShelleyBasedEra era -> TxBundle era -> SomeTxBundle

instance Binary SomeTxBundle where
  put (SomeTxBundle era TxBundle{..}) = do
    let hEra = AnyShelleyBasedEra era
    let hGroupTabSize = fromIntegral $ U.length tbGroupTab
    let hSigTabSize = fromIntegral $ U.length tbSigTab
    let hGroupMemTabSize = fromIntegral $ U.length tbGroupMemTab
    let utxo = unUTxO tbUtxo
    let hUtxoTabSize = fromIntegral $ Map.size utxo
    let summaryBytes = BS.toStrict $ give EraParamsWithGenesisWindow $ serialise tbSummary
    let pparamsBytes = case era of
          ShelleyBasedEraConway -> serialize' $ unLedgerProtocolParameters tbPParams
          ShelleyBasedEraBabbage -> serialize' $ unLedgerProtocolParameters tbPParams
          ShelleyBasedEraAlonzo -> serialize' $ unLedgerProtocolParameters tbPParams
          ShelleyBasedEraMary -> serialize' $ unLedgerProtocolParameters tbPParams
          ShelleyBasedEraAllegra -> serialize' $ unLedgerProtocolParameters tbPParams
          ShelleyBasedEraShelley -> serialize' $ unLedgerProtocolParameters tbPParams
    let txBodyBytes = withShelleyBasedEra era $ serialiseToCBOR tbTxBody
    let hSummarySize = BS.length summaryBytes
    let hPParamsSize = BS.length pparamsBytes
    let hTxBodySize = BS.length txBodyBytes
    put TxBundleHeader{..}
    U.mapM_ put tbGroupTab
    U.mapM_ put tbSigTab
    U.mapM_ put tbGroupMemTab
    utxoBytes <- for (Map.toAscList utxo) \(TxIn utxoTxId utxoTxIx, txOut) -> do
      let txOutBytes = case era of
            ShelleyBasedEraConway -> serialize' $ toShelleyTxOut era txOut
            ShelleyBasedEraBabbage -> serialize' $ toShelleyTxOut era txOut
            ShelleyBasedEraAlonzo -> serialize' $ toShelleyTxOut era txOut
            ShelleyBasedEraMary -> serialize' $ toShelleyTxOut era txOut
            ShelleyBasedEraAllegra -> serialize' $ toShelleyTxOut era txOut
            ShelleyBasedEraShelley -> serialize' $ toShelleyTxOut era txOut
      let utxoTxOutSize = BS.length txOutBytes
      put UtxoHeader{..}
      pure txOutBytes
    put $
      nominalDiffTimeToSeconds $
        utcTimeToPOSIXSeconds $
          getSystemStart tbSystemStart
    putByteString summaryBytes
    putByteString pparamsBytes
    traverse_ putByteString utxoBytes
    putByteString txBodyBytes
  get = do
    TxBundleHeader{..} <- get
    AnyShelleyBasedEra era <- pure hEra
    tbGroupTab <- U.replicateM (fromIntegral hGroupTabSize) get
    tbSigTab <- U.replicateM (fromIntegral hSigTabSize) get
    tbGroupMemTab <- U.replicateM (fromIntegral hGroupMemTabSize) get
    utxoTab <- U.replicateM (fromIntegral hUtxoTabSize) get
    tbSystemStart <-
      SystemStart . posixSecondsToUTCTime . secondsToNominalDiffTime <$> get
    tbSummary <- give EraParamsWithGenesisWindow do
      bytes <- getByteString hSummarySize
      either (fail . show) pure $ deserialiseOrFail $ BS.fromStrict bytes
    tbPParams <- getPParams era hPParamsSize
    tbUtxo <- UTxO . Map.fromList <$> traverse (getUtxo era) (U.toList utxoTab)
    tbTxBody <- getTxBody era hTxBodySize
    pure $ SomeTxBundle era TxBundle{..}

getPParams
  :: forall era. ShelleyBasedEra era -> Int -> Get (LedgerProtocolParameters era)
getPParams era size = do
  bytes <- getByteString size
  let txOutResult :: Either DecoderError (LedgerProtocolParameters era)
      txOutResult = case era of
        ShelleyBasedEraConway -> LedgerProtocolParameters <$> decodeFull' bytes
        ShelleyBasedEraBabbage -> LedgerProtocolParameters <$> decodeFull' bytes
        ShelleyBasedEraAlonzo -> LedgerProtocolParameters <$> decodeFull' bytes
        ShelleyBasedEraMary -> LedgerProtocolParameters <$> decodeFull' bytes
        ShelleyBasedEraAllegra -> LedgerProtocolParameters <$> decodeFull' bytes
        ShelleyBasedEraShelley -> LedgerProtocolParameters <$> decodeFull' bytes
  either (fail . show) pure txOutResult

getUtxo
  :: forall era. ShelleyBasedEra era -> UtxoHeader -> Get (TxIn, TxOut CtxUTxO era)
getUtxo era UtxoHeader{..} = do
  bytes <- getByteString utxoTxOutSize
  let txOutResult :: Either DecoderError (TxOut CtxUTxO era)
      txOutResult = case era of
        ShelleyBasedEraConway -> fromShelleyTxOut ShelleyBasedEraConway <$> decodeFull' bytes
        ShelleyBasedEraBabbage -> fromShelleyTxOut ShelleyBasedEraBabbage <$> decodeFull' bytes
        ShelleyBasedEraAlonzo -> fromShelleyTxOut ShelleyBasedEraAlonzo <$> decodeFull' bytes
        ShelleyBasedEraMary -> fromShelleyTxOut ShelleyBasedEraMary <$> decodeFull' bytes
        ShelleyBasedEraAllegra -> fromShelleyTxOut ShelleyBasedEraAllegra <$> decodeFull' bytes
        ShelleyBasedEraShelley -> fromShelleyTxOut ShelleyBasedEraShelley <$> decodeFull' bytes
  either (fail . show) (pure . (TxIn utxoTxId utxoTxIx,)) txOutResult

getTxBody :: forall era. ShelleyBasedEra era -> Int -> Get (TxBody era)
getTxBody era size = do
  bytes <- getByteString size
  withShelleyBasedEra era $
    either (fail . show) pure $
      deserialiseFromCBOR (AsTxBody $ proxyToAsType $ Proxy @era) bytes

withShelleyBasedEra
  :: ShelleyBasedEra era -> ((IsShelleyBasedEra era) => a) -> a
withShelleyBasedEra = \case
  ShelleyBasedEraConway -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraAlonzo -> id
  ShelleyBasedEraMary -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraShelley -> id

instance Binary AnyShelleyBasedEra where
  put (AnyShelleyBasedEra era) = putWord8 case era of
    ShelleyBasedEraShelley -> 0
    ShelleyBasedEraAllegra -> 1
    ShelleyBasedEraMary -> 2
    ShelleyBasedEraAlonzo -> 3
    ShelleyBasedEraBabbage -> 4
    ShelleyBasedEraConway -> 5
  get =
    getWord8 >>= \case
      0 -> pure $ AnyShelleyBasedEra ShelleyBasedEraShelley
      1 -> pure $ AnyShelleyBasedEra ShelleyBasedEraAllegra
      2 -> pure $ AnyShelleyBasedEra ShelleyBasedEraMary
      3 -> pure $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
      4 -> pure $ AnyShelleyBasedEra ShelleyBasedEraBabbage
      5 -> pure $ AnyShelleyBasedEra ShelleyBasedEraConway
      n -> fail $ "Invalid era byte value " <> show n

instance Binary (Hash PaymentKey) where
  put = put . pkhToBlake2b_244_Unpacked
  get =
    PaymentKeyHash . KeyHash . hashFromPackedBytes
      <$> (PackedBytes28 <$> get <*> get <*> get <*> get)

instance Binary TxId where
  put = put . txIdToBlake2b_256_Unpacked
  get =
    TxId . hashFromPackedBytes <$> (PackedBytes32 <$> get <*> get <*> get <*> get)

deriving newtype instance Binary TxIx

newtype instance U.MVector s GroupHeader = MV_GroupHeader (U.MVector s (Word16, Word16))
newtype instance U.Vector GroupHeader = V_GroupHeader (U.Vector (Word16, Word16))

instance M.MVector U.MVector GroupHeader where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ M.basicLength @U.MVector @(Word16, Word16)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(Word16, Word16)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(Word16, Word16)
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(Word16, Word16)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(Word16, Word16)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(Word16, Word16)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(Word16, Word16)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(Word16, Word16)
  basicClear = coerce $ M.basicClear @U.MVector @(Word16, Word16)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n GroupHeader{..} =
    MV_GroupHeader <$> M.basicUnsafeReplicate n (grpSize, grpThreshold)
  basicUnsafeRead (MV_GroupHeader v) i =
    uncurry GroupHeader <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_GroupHeader v) i GroupHeader{..} =
    M.basicUnsafeWrite v i (grpSize, grpThreshold)
  basicSet (MV_GroupHeader v) GroupHeader{..} =
    M.basicSet v (grpSize, grpThreshold)

instance G.Vector Vector GroupHeader where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(Word16, Word16)
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @(Word16, Word16)
  basicLength = coerce $ G.basicLength @Vector @(Word16, Word16)
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @(Word16, Word16)
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @(Word16, Word16)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_GroupHeader v) i =
    uncurry GroupHeader <$> G.basicUnsafeIndexM v i
  elemseq _ GroupHeader{..} z =
    G.elemseq (undefined :: Vector Word16) grpSize $
      G.elemseq (undefined :: Vector Word16) grpThreshold z

instance U.Unbox GroupHeader

newtype instance U.MVector s GroupMemHeader = MV_GroupMemHeader (U.MVector s (Word16, Word16))
newtype instance U.Vector GroupMemHeader = V_GroupMemHeader (U.Vector (Word16, Word16))

instance M.MVector U.MVector GroupMemHeader where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ M.basicLength @U.MVector @(Word16, Word16)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(Word16, Word16)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(Word16, Word16)
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(Word16, Word16)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(Word16, Word16)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(Word16, Word16)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(Word16, Word16)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(Word16, Word16)
  basicClear = coerce $ M.basicClear @U.MVector @(Word16, Word16)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n GroupMemHeader{..} =
    MV_GroupMemHeader <$> M.basicUnsafeReplicate n (gmGroup, gmSig)
  basicUnsafeRead (MV_GroupMemHeader v) i =
    uncurry GroupMemHeader <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_GroupMemHeader v) i GroupMemHeader{..} =
    M.basicUnsafeWrite v i (gmGroup, gmSig)
  basicSet (MV_GroupMemHeader v) GroupMemHeader{..} =
    M.basicSet v (gmGroup, gmSig)

instance G.Vector Vector GroupMemHeader where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(Word16, Word16)
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @(Word16, Word16)
  basicLength = coerce $ G.basicLength @Vector @(Word16, Word16)
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @(Word16, Word16)
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @(Word16, Word16)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_GroupMemHeader v) i =
    uncurry GroupMemHeader <$> G.basicUnsafeIndexM v i
  elemseq _ GroupMemHeader{..} z =
    G.elemseq (undefined :: Vector Word16) gmGroup $
      G.elemseq (undefined :: Vector Word16) gmSig z

instance U.Unbox GroupMemHeader

newtype instance U.MVector s UtxoHeader = MV_UtxoHeader (U.MVector s (TxId, TxIx, Int))
newtype instance U.Vector UtxoHeader = V_UtxoHeader (U.Vector (TxId, TxIx, Int))

instance M.MVector U.MVector UtxoHeader where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ M.basicLength @U.MVector @(TxId, TxIx, Int)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(TxId, TxIx, Int)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(TxId, TxIx, Int)
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(TxId, TxIx, Int)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(TxId, TxIx, Int)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(TxId, TxIx, Int)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(TxId, TxIx, Int)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(TxId, TxIx, Int)
  basicClear = coerce $ M.basicClear @U.MVector @(TxId, TxIx, Int)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n UtxoHeader{..} =
    MV_UtxoHeader <$> M.basicUnsafeReplicate n (utxoTxId, utxoTxIx, utxoTxOutSize)
  basicUnsafeRead (MV_UtxoHeader v) i =
    uncurry3 UtxoHeader <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UtxoHeader v) i UtxoHeader{..} =
    M.basicUnsafeWrite v i (utxoTxId, utxoTxIx, utxoTxOutSize)
  basicSet (MV_UtxoHeader v) UtxoHeader{..} =
    M.basicSet v (utxoTxId, utxoTxIx, utxoTxOutSize)

instance G.Vector Vector UtxoHeader where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(TxId, TxIx, Int)
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @(TxId, TxIx, Int)
  basicLength = coerce $ G.basicLength @Vector @(TxId, TxIx, Int)
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @(TxId, TxIx, Int)
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @(TxId, TxIx, Int)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_UtxoHeader v) i =
    uncurry3 UtxoHeader <$> G.basicUnsafeIndexM v i
  elemseq _ UtxoHeader{..} z =
    G.elemseq (undefined :: Vector TxId) utxoTxId $
      G.elemseq (undefined :: Vector TxIx) utxoTxIx $
        G.elemseq (undefined :: Vector Int) utxoTxOutSize z

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

instance U.Unbox UtxoHeader

type Blake2b_244_Unpacked = (Word64, Word64, Word64, Word32)

newtype instance U.MVector s (Hash PaymentKey)
  = MV_Hash_PaymentKey (U.MVector s Blake2b_244_Unpacked)
newtype instance U.Vector (Hash PaymentKey) = V_Hash_PaymentKey (U.Vector Blake2b_244_Unpacked)

instance M.MVector U.MVector (Hash PaymentKey) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ M.basicLength @U.MVector @Blake2b_244_Unpacked
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @Blake2b_244_Unpacked
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @Blake2b_244_Unpacked
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @Blake2b_244_Unpacked
  basicInitialize = coerce $ M.basicInitialize @U.MVector @Blake2b_244_Unpacked
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @Blake2b_244_Unpacked
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @Blake2b_244_Unpacked
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @Blake2b_244_Unpacked
  basicClear = coerce $ M.basicClear @U.MVector @Blake2b_244_Unpacked
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n =
    fmap MV_Hash_PaymentKey . M.basicUnsafeReplicate n . pkhToBlake2b_244_Unpacked
  basicUnsafeRead (MV_Hash_PaymentKey v) i = do
    (a, b, c, d) <- M.basicUnsafeRead v i
    pure $ PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 a b c d
  basicUnsafeWrite (MV_Hash_PaymentKey v) i =
    M.basicUnsafeWrite v i . pkhToBlake2b_244_Unpacked
  basicSet (MV_Hash_PaymentKey v) =
    M.basicSet v . pkhToBlake2b_244_Unpacked

pkhToBlake2b_244_Unpacked :: Hash PaymentKey -> Blake2b_244_Unpacked
pkhToBlake2b_244_Unpacked (PaymentKeyHash (KeyHash kh)) = case hashToPackedBytes kh of
  PackedBytes28 a b c d -> (a, b, c, d)
  _ -> case hashToBytesShort kh of
    (SBS ba#) ->
      let ba = ByteArray ba#
       in ( indexWord64BE ba 0
          , indexWord64BE ba 8
          , indexWord64BE ba 16
          , indexWord32BE ba 28
          )

instance G.Vector Vector (Hash PaymentKey) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @Blake2b_244_Unpacked
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @Blake2b_244_Unpacked
  basicLength = coerce $ G.basicLength @Vector @Blake2b_244_Unpacked
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @Blake2b_244_Unpacked
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @Blake2b_244_Unpacked
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_Hash_PaymentKey v) i = do
    (a, b, c, d) <- G.basicUnsafeIndexM v i
    pure $ PaymentKeyHash $ KeyHash $ hashFromPackedBytes $ PackedBytes28 a b c d
  elemseq _ (pkhToBlake2b_244_Unpacked -> (a, b, c, d)) z =
    G.elemseq (undefined :: Vector Word64) a $
      G.elemseq (undefined :: Vector Word64) b $
        G.elemseq (undefined :: Vector Word64) c $
          G.elemseq (undefined :: Vector Word32) d z

instance U.Unbox (Hash PaymentKey)

type Blake2b_256_Unpacked = (Word64, Word64, Word64, Word64)

newtype instance U.MVector s TxId = MV_TxId (U.MVector s Blake2b_256_Unpacked)
newtype instance U.Vector TxId = V_TxId (U.Vector Blake2b_256_Unpacked)

instance M.MVector U.MVector TxId where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ M.basicLength @U.MVector @Blake2b_256_Unpacked
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @Blake2b_256_Unpacked
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @Blake2b_256_Unpacked
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @Blake2b_256_Unpacked
  basicInitialize = coerce $ M.basicInitialize @U.MVector @Blake2b_256_Unpacked
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @Blake2b_256_Unpacked
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @Blake2b_256_Unpacked
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @Blake2b_256_Unpacked
  basicClear = coerce $ M.basicClear @U.MVector @Blake2b_256_Unpacked
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n =
    fmap MV_TxId . M.basicUnsafeReplicate n . txIdToBlake2b_256_Unpacked
  basicUnsafeRead (MV_TxId v) i = do
    (a, b, c, d) <- M.basicUnsafeRead v i
    pure $ TxId $ hashFromPackedBytes $ PackedBytes32 a b c d
  basicUnsafeWrite (MV_TxId v) i =
    M.basicUnsafeWrite v i . txIdToBlake2b_256_Unpacked
  basicSet (MV_TxId v) =
    M.basicSet v . txIdToBlake2b_256_Unpacked

txIdToBlake2b_256_Unpacked :: TxId -> Blake2b_256_Unpacked
txIdToBlake2b_256_Unpacked (TxId hash) = case hashToPackedBytes hash of
  PackedBytes32 a b c d -> (a, b, c, d)
  _ -> case hashToBytesShort hash of
    (SBS ba#) ->
      let ba = ByteArray ba#
       in ( indexWord64BE ba 0
          , indexWord64BE ba 8
          , indexWord64BE ba 16
          , indexWord64BE ba 28
          )

instance G.Vector Vector TxId where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @Blake2b_256_Unpacked
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @Blake2b_256_Unpacked
  basicLength = coerce $ G.basicLength @Vector @Blake2b_256_Unpacked
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @Blake2b_256_Unpacked
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @Blake2b_256_Unpacked
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_TxId v) i = do
    (a, b, c, d) <- G.basicUnsafeIndexM v i
    pure $ TxId $ hashFromPackedBytes $ PackedBytes32 a b c d
  elemseq _ (txIdToBlake2b_256_Unpacked -> (a, b, c, d)) z =
    G.elemseq (undefined :: Vector Word64) a $
      G.elemseq (undefined :: Vector Word64) b $
        G.elemseq (undefined :: Vector Word64) c $
          G.elemseq (undefined :: Vector Word64) d z

instance U.Unbox TxId

newtype instance U.MVector s TxIx = MV_TxIx (U.MVector s Word)
newtype instance U.Vector TxIx = V_TxIx (U.Vector Word)
deriving newtype instance M.MVector U.MVector TxIx
deriving newtype instance G.Vector Vector TxIx
instance U.Unbox TxIx
