{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.TxDynamic where

import Cardano.Api (
  AnyShelleyBasedEra (..),
  AsType (..),
  HasTypeProxy (proxyToAsType),
  Hash,
  IsShelleyBasedEra,
  Key (..),
  MonadTrans (..),
  PaymentKey,
  SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR),
  ShelleyBasedEra (..),
  SigningKey (..),
  Tx,
  TxBody,
  TxId (..),
  TxIx (..),
  TxScriptValidity,
  VerificationKey (..),
  runExceptT,
  throwE,
  txScriptValidityToScriptValidity,
 )
import qualified Cardano.Api as C
import Cardano.Api.Ledger (
  KeyHash (..),
  StandardCrypto,
  VKey (..),
  extractHash,
  maybeToStrictMaybe,
 )
import qualified Cardano.Api.Ledger as Ledger
import Cardano.Api.Shelley (
  Hash (..),
  ShelleyLedgerEra,
  Tx (ShelleyTx),
 )
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  Ed25519DSIGN,
  SigDSIGN (SigEd25519DSIGN),
  SignedDSIGN (..),
  VerKeyDSIGN (VerKeyEd25519DSIGN),
 )
import Cardano.Crypto.Hash (
  PackedBytes (..),
  hashFromPackedBytes,
  hashToBytesShort,
  hashToPackedBytes,
 )
import Cardano.Crypto.Libsodium.C (CRYPTO_SIGN_ED25519_PUBLICKEYBYTES)
import Cardano.Crypto.Libsodium.Constants (CRYPTO_SIGN_ED25519_BYTES)
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Cardano.Ledger.Alonzo.Core (
  AlonzoEraTxWits (..),
  hashScript,
  rdmrsTxWitsL,
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoEraTx (..),
  AlonzoTxBody (..),
  IsValid (..),
 )
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Babbage.Tx (BabbageTxBody (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core (EraIndependentTxBody, EraTx)
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.Keys as Shelley
import Cardano.Ledger.SafeHash (HashAnnotated (..))
import Control.Lens ((.~))
import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.Array.Byte (ByteArray (ByteArray))
import Data.Bifunctor (Bifunctor (..))
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
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.Data (Proxy (..), Typeable)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64, Word8)
import Data.Word.Extra (indexWord32BE, indexWord64BE)
import GHC.Generics (Generic)
import GHC.TypeLits (natVal)
import Unsafe.Coerce (unsafeCoerce)

-- | Header in a tx bundle file
data TxBundleHeader = TxBundleHeader
  { hEra :: AnyShelleyBasedEra
  -- ^ The era of the transaction
  , hGroupTabSize :: Word8
  -- ^ The length of the group table (in rows)
  , hSigTabSize :: Word8
  -- ^ The length of the signatory table (in rows)
  , hGroupMemTabSize :: Word8
  -- ^ The length of the group membership table (in rows)
  , hTxBodySize :: Int
  -- ^ The size of the tx body (in bytes)
  }
  deriving (Show, Eq, Generic, Binary)

-- | A row in the group table
data GroupHeader = GroupHeader
  { grpSize :: Word8
  -- ^ The number of signatories in the group
  , grpThreshold :: Word8
  -- ^ The minimum number of signatures required from the group
  }
  deriving (Show, Eq, Generic, Binary)

-- | A row in the group membership table
data GroupMemHeader = GroupMemHeader
  { gmGroup :: Word8
  -- ^ The group to which the signatory belongs
  , gmSig :: Word8
  -- ^ The signatory that belongs to the group
  }
  deriving (Show, Eq, Generic, Binary)

data TxBundle era = TxBundle
  { tbGroupTab :: Vector GroupHeader
  , tbSigTab :: Vector (Hash PaymentKey)
  , tbGroupMemTab :: Vector GroupMemHeader
  , tbGroupNameTab :: V.Vector Text
  , tbTxBody :: TxBody era
  }
  deriving (Show, Eq)

data GroupError
  = GroupIndexOutOufBounds Word8
  | SignatoryIndexOutOufBounds Word8
  | GroupSizeWrong Word8 Int
  | GroupThresholdTooHigh Word8 Word8
  deriving (Show, Eq)

data SignBundleError
  = SignGroupError GroupError
  | SignBadEra
  | SignNoTransactions
  deriving (Show, Eq)

data AssembleError
  = AssembleGroupError GroupError
  | AssembleBadEra
  | TooFewSignatures
  | MissingSignature (Hash PaymentKey) TxId
  deriving (Show, Eq)

data WitnessBundle = WitnessBundle
  { wbVerificationKey :: VerificationKey PaymentKey
  , wbSignatures :: Map TxId (SigDSIGN Ed25519DSIGN)
  }
  deriving (Show, Eq, Generic, Binary)

assemble :: [WitnessBundle] -> TxBundle era -> Either AssembleError (Tx era)
assemble witBundles bundle = do
  groups <- first AssembleGroupError $ getSigningGroups bundle
  let allGroupSigners = U.foldMap Set.singleton $ tbSigTab bundle
  let signedGroupSigners =
        Set.intersection allGroupSigners $
          foldMap (Set.singleton . verificationKeyHash . wbVerificationKey) witBundles
  void $ flip Map.traverseWithKey groups \group threshold ->
    when (Set.size (Set.intersection group signedGroupSigners) < threshold) $
      Left TooFewSignatures
  txBody <- setSigners AssembleBadEra signedGroupSigners $ tbTxBody bundle
  let txId = C.getTxId txBody
  makeSignedTransaction txBody <$> for witBundles \WitnessBundle{..} -> do
    let pkh = verificationKeyHash wbVerificationKey
    let PaymentVerificationKey (VKey vkey) = wbVerificationKey
    case Map.lookup txId wbSignatures of
      Nothing -> Left $ MissingSignature pkh txId
      Just signature -> pure $ Shelley.WitVKey (VKey vkey) (SignedDSIGN signature)

-- We need to copy this from cardano-api because it doesn't expose KeyWitness
-- constructors.
makeSignedTransaction
  :: forall era
   . TxBody era
  -> [Shelley.WitVKey Shelley.Witness StandardCrypto]
  -> Tx era
makeSignedTransaction
  ( C.ShelleyTxBody
      sbe
      txBody
      txScripts
      txScriptData
      txMetadata
      scriptValidity
    )
  witnesses =
    case sbe of
      ShelleyBasedEraShelley -> shelleySignedTransaction
      ShelleyBasedEraAllegra -> shelleySignedTransaction
      ShelleyBasedEraMary -> shelleySignedTransaction
      ShelleyBasedEraAlonzo -> alonzoSignedTransaction
      ShelleyBasedEraBabbage -> alonzoSignedTransaction
      ShelleyBasedEraConway -> alonzoSignedTransaction
    where
      txCommon
        :: forall lEra
         . (ShelleyLedgerEra era ~ lEra)
        => (L.EraCrypto lEra ~ StandardCrypto)
        => (EraTx lEra)
        => L.Tx lEra
      txCommon =
        L.mkBasicTx txBody
          & L.witsTxL
            .~ ( L.mkBasicTxWits
                  & L.addrTxWitsL
                    .~ Set.fromList witnesses
                  & L.scriptTxWitsL
                    .~ Map.fromList
                      [ (hashScript @lEra sw, sw)
                      | sw <- txScripts
                      ]
               )
          & L.auxDataTxL
            .~ maybeToStrictMaybe txMetadata

      shelleySignedTransaction
        :: forall lEra
         . (ShelleyLedgerEra era ~ lEra)
        => (Ledger.EraCrypto lEra ~ StandardCrypto)
        => (EraTx lEra)
        => Tx era
      shelleySignedTransaction = ShelleyTx sbe txCommon

      alonzoSignedTransaction
        :: forall lEra
         . (ShelleyLedgerEra era ~ lEra)
        => (Ledger.EraCrypto lEra ~ StandardCrypto)
        => (AlonzoEraTx lEra)
        => Tx era
      alonzoSignedTransaction =
        ShelleyTx
          sbe
          ( txCommon
              & L.witsTxL
                . datsTxWitsL
                .~ datums
              & L.witsTxL
                . rdmrsTxWitsL
                .~ redeemers
              & isValidTxL
                .~ txScriptValidityToIsValid scriptValidity
          )
        where
          (datums, redeemers) =
            case txScriptData of
              C.TxBodyScriptData _ ds rs -> (ds, rs)
              C.TxBodyNoScriptData -> (mempty, Redeemers mempty)

txScriptValidityToIsValid :: TxScriptValidity era -> IsValid
txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

scriptValidityToIsValid :: C.ScriptValidity -> IsValid
scriptValidityToIsValid C.ScriptInvalid = IsValid False
scriptValidityToIsValid C.ScriptValid = IsValid True

signBundle
  :: forall era
   . SigningKey PaymentKey
  -> TxBundle era
  -> Either SignBundleError WitnessBundle
signBundle sk bundle@TxBundle{..} = do
  combinations <- first SignGroupError $ signatureCombinations bundle
  let vk = getVerificationKey sk
  let pkh = verificationKeyHash vk
  let filtered = Set.filter (Set.member pkh) combinations
  when (Set.null filtered) $ Left SignNoTransactions
  WitnessBundle vk . Map.fromList <$> for (Set.toList filtered) \signers -> do
    txBody <- setSigners SignBadEra signers tbTxBody
    let C.ShelleyTxBody era ledgerBody' _ _ _ _ = txBody
    let txHash :: Shelley.Hash StandardCrypto EraIndependentTxBody
        txHash =
          C.shelleyBasedEraConstraints era $
            extractHash @StandardCrypto $
              hashAnnotated ledgerBody'
    case sk of
      PaymentSigningKey sk' -> pure (C.getTxId txBody, signDSIGN () txHash sk')

signBundleAll
  :: forall era
   . SigningKey PaymentKey
  -> TxBundle era
  -> Either SignBundleError WitnessBundle
signBundleAll sk bundle@TxBundle{..} = do
  combinations <- first SignGroupError $ signatureCombinations bundle
  let vk = getVerificationKey sk
  WitnessBundle vk . Map.fromList <$> for (Set.toList combinations) \signers -> do
    txBody <- setSigners SignBadEra signers tbTxBody
    let C.ShelleyTxBody era ledgerBody' _ _ _ _ = txBody
    let txHash :: Shelley.Hash StandardCrypto EraIndependentTxBody
        txHash =
          C.shelleyBasedEraConstraints era $
            extractHash @StandardCrypto $
              hashAnnotated ledgerBody'
    case sk of
      PaymentSigningKey sk' -> pure (C.getTxId txBody, signDSIGN () txHash sk')

setSigners
  :: e
  -> Set (Hash PaymentKey)
  -> TxBody era
  -> Either e (TxBody era)
setSigners err signers (C.ShelleyTxBody era ledgerBody scripts scriptData auxData validity) = do
  ledgerBody' <- case era of
    ShelleyBasedEraShelley -> Left err
    ShelleyBasedEraAllegra -> Left err
    ShelleyBasedEraMary -> Left err
    ShelleyBasedEraAlonzo ->
      case ledgerBody of
        AlonzoTxBody{..} ->
          pure
            AlonzoTxBody
              { atbReqSignerHashes = Set.mapMonotonic coerce signers
              , ..
              }
    ShelleyBasedEraBabbage ->
      case ledgerBody of
        BabbageTxBody{..} ->
          pure
            BabbageTxBody
              { btbReqSignerHashes = Set.mapMonotonic coerce signers
              , ..
              }
    ShelleyBasedEraConway ->
      case ledgerBody of
        ConwayTxBody{..} ->
          pure
            ConwayTxBody
              { ctbReqSignerHashes = Set.mapMonotonic coerce signers
              , ..
              }
  pure $ C.ShelleyTxBody era ledgerBody' scripts scriptData auxData validity

signatureCombinations
  :: TxBundle era -> Either GroupError (Set (Set (Hash PaymentKey)))
signatureCombinations bundle = do
  groups <- getSigningGroups bundle
  pure $ Map.foldlWithKey' mergeCombinations (Set.singleton mempty) groups

getSigningGroups
  :: TxBundle era -> Either GroupError (Map (Set (Hash PaymentKey)) Int)
getSigningGroups TxBundle{..} = do
  runST $ runExceptT do
    groups <- lift $ V.replicateM (U.length tbGroupTab) $ newSTRef Set.empty
    let groupCount = U.length tbGroupTab
    let sigCount = U.length tbSigTab
    U.forM_ tbGroupMemTab \GroupMemHeader{..} -> do
      let groupIx = fromIntegral gmGroup
      let sigIx = fromIntegral gmSig
      when (groupIx >= groupCount) $ throwE $ GroupIndexOutOufBounds gmGroup
      when (sigIx >= sigCount) $ throwE $ SignatoryIndexOutOufBounds gmSig
      let groupRef = groups V.! groupIx
      let pkh = tbSigTab U.! sigIx
      lift $ modifySTRef groupRef $ Set.insert pkh
    Map.fromListWith max <$> for [0 .. groupCount - 1] \i -> do
      let GroupHeader{..} = tbGroupTab U.! i
      let groupRef = groups V.! i
      group <- lift $ readSTRef groupRef
      unless (Set.size group == fromIntegral grpSize) $
        throwE $
          GroupSizeWrong grpSize $
            Set.size group
      when (grpThreshold > grpSize) $
        throwE $
          GroupThresholdTooHigh grpThreshold grpSize
      pure (group, fromIntegral grpThreshold)

mergeCombinations
  :: Set (Set (Hash PaymentKey))
  -> Set (Hash PaymentKey)
  -> Int
  -> Set (Set (Hash PaymentKey))
mergeCombinations prev group threshold =
  Set.map (uncurry Set.union) $
    Set.cartesianProduct prev (groupCombinations threshold group)

groupCombinations :: Int -> Set (Hash PaymentKey) -> Set (Set (Hash PaymentKey))
groupCombinations threshold = Set.filter (\s -> Set.size s >= threshold) . Set.powerSet

data SomeTxBundle where
  SomeTxBundle
    :: (Typeable era) => ShelleyBasedEra era -> TxBundle era -> SomeTxBundle

instance Binary SomeTxBundle where
  put (SomeTxBundle era TxBundle{..}) = do
    let hEra = AnyShelleyBasedEra era
    let hGroupTabSize = fromIntegral $ U.length tbGroupTab
    let hSigTabSize = fromIntegral $ U.length tbSigTab
    let hGroupMemTabSize = fromIntegral $ U.length tbGroupMemTab
    let txBodyBytes = withShelleyBasedEra era $ serialiseToCBOR tbTxBody
    let hTxBodySize = BS.length txBodyBytes
    put TxBundleHeader{..}
    U.mapM_ put tbGroupTab
    U.mapM_ put tbSigTab
    U.mapM_ put tbGroupMemTab
    traverse_ put tbGroupNameTab
    putByteString txBodyBytes
  get = do
    TxBundleHeader{..} <- get
    AnyShelleyBasedEra era <- pure hEra
    tbGroupTab <- U.replicateM (fromIntegral hGroupTabSize) get
    tbSigTab <- U.replicateM (fromIntegral hSigTabSize) get
    tbGroupMemTab <- U.replicateM (fromIntegral hGroupMemTabSize) get
    tbGroupNameTab <- V.replicateM (U.length tbGroupTab) get
    tbTxBody <- getTxBody era hTxBodySize
    pure $ SomeTxBundle era TxBundle{..}

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

newtype instance U.MVector s GroupHeader = MV_GroupHeader (U.MVector s (Word8, Word8))
newtype instance U.Vector GroupHeader = V_GroupHeader (U.Vector (Word8, Word8))

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
  basicLength = coerce $ M.basicLength @U.MVector @(Word8, Word8)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(Word8, Word8)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(Word8, Word8)
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(Word8, Word8)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(Word8, Word8)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(Word8, Word8)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(Word8, Word8)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(Word8, Word8)
  basicClear = coerce $ M.basicClear @U.MVector @(Word8, Word8)
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
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(Word8, Word8)
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @(Word8, Word8)
  basicLength = coerce $ G.basicLength @Vector @(Word8, Word8)
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @(Word8, Word8)
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @(Word8, Word8)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_GroupHeader v) i =
    uncurry GroupHeader <$> G.basicUnsafeIndexM v i
  elemseq _ GroupHeader{..} z =
    G.elemseq (undefined :: Vector Word8) grpSize $
      G.elemseq (undefined :: Vector Word8) grpThreshold z

instance U.Unbox GroupHeader

newtype instance U.MVector s GroupMemHeader = MV_GroupMemHeader (U.MVector s (Word8, Word8))
newtype instance U.Vector GroupMemHeader = V_GroupMemHeader (U.Vector (Word8, Word8))

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
  basicLength = coerce $ M.basicLength @U.MVector @(Word8, Word8)
  basicUnsafeSlice = coerce $ M.basicUnsafeSlice @U.MVector @(Word8, Word8)
  basicOverlaps = coerce $ M.basicOverlaps @U.MVector @(Word8, Word8)
  basicUnsafeNew = coerce $ M.basicUnsafeNew @U.MVector @(Word8, Word8)
  basicInitialize = coerce $ M.basicInitialize @U.MVector @(Word8, Word8)
  basicUnsafeCopy = coerce $ M.basicUnsafeCopy @U.MVector @(Word8, Word8)
  basicUnsafeMove = coerce $ M.basicUnsafeMove @U.MVector @(Word8, Word8)
  basicUnsafeGrow = coerce $ M.basicUnsafeGrow @U.MVector @(Word8, Word8)
  basicClear = coerce $ M.basicClear @U.MVector @(Word8, Word8)
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
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @Vector @(Word8, Word8)
  basicUnsafeThaw = coerce $ G.basicUnsafeThaw @Vector @(Word8, Word8)
  basicLength = coerce $ G.basicLength @Vector @(Word8, Word8)
  basicUnsafeSlice = coerce $ G.basicUnsafeSlice @Vector @(Word8, Word8)
  basicUnsafeCopy = coerce $ G.basicUnsafeCopy @Vector @(Word8, Word8)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_GroupMemHeader v) i =
    uncurry GroupMemHeader <$> G.basicUnsafeIndexM v i
  elemseq _ GroupMemHeader{..} z =
    G.elemseq (undefined :: Vector Word8) gmGroup $
      G.elemseq (undefined :: Vector Word8) gmSig z

instance U.Unbox GroupMemHeader

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

instance Binary (VerificationKey PaymentKey) where
  put (PaymentVerificationKey (VKey (VerKeyEd25519DSIGN bytes))) = do
    putByteString $ psbToByteString bytes
  get = do
    SBS bytes <-
      SBS.toShort
        <$> getByteString (fromInteger $ natVal $ Proxy @CRYPTO_SIGN_ED25519_PUBLICKEYBYTES)
    pure
      . PaymentVerificationKey
      . VKey
      . VerKeyEd25519DSIGN
      $ ( unsafeCoerce
            :: (ByteArray -> PinnedSizedBytes CRYPTO_SIGN_ED25519_PUBLICKEYBYTES)
        )
      $ ByteArray bytes

instance Binary (SigDSIGN Ed25519DSIGN) where
  put (SigEd25519DSIGN bytes) = do
    putByteString $ psbToByteString bytes
  get = do
    SBS bytes <-
      SBS.toShort
        <$> getByteString (fromInteger $ natVal $ Proxy @CRYPTO_SIGN_ED25519_BYTES)
    pure
      . SigEd25519DSIGN
      $ (unsafeCoerce :: (ByteArray -> PinnedSizedBytes CRYPTO_SIGN_ED25519_BYTES))
      $ ByteArray bytes
