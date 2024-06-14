{-# OPTIONS_GHC -Wno-orphans #-}

module CredentialManager.Gen where

import Control.Monad (guard, replicateM, (<=<))
import CredentialManager.Api
import qualified CredentialManager.Scripts.ColdCommittee as CC
import qualified CredentialManager.Scripts.ColdNFT as CN
import qualified CredentialManager.Scripts.HotCommittee as HC
import qualified CredentialManager.Scripts.HotNFT as HN
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.Foldable (Foldable (..))
import Data.List (iterate')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (Sum (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import PlutusLedgerApi.V3
import qualified PlutusTx.AssocMap as AMap
import Test.QuickCheck

newtype ArbitraryHash (n :: Nat) = ArbitraryHash {getArbitraryHash :: BuiltinByteString}

arbitraryHashFromSeed :: forall n. (KnownNat n) => Word8 -> ArbitraryHash n
arbitraryHashFromSeed = ArbitraryHash . toBuiltin . BS.pack . take n . iterate (`rotateL` 1)
  where
    n = fromIntegral $ natVal $ Proxy @n

instance (KnownNat n) => Arbitrary (ArbitraryHash n) where
  arbitrary = arbitraryHashFromSeed <$> arbitrary
  shrink = fmap arbitraryHashFromSeed . shrink <=< getSeed

getSeed :: ArbitraryHash n -> [Word8]
getSeed (ArbitraryHash hash)
  | BS.null bs = []
  | otherwise = pure $ BS.head bs
  where
    bs = fromBuiltin hash

shrinkHash :: BuiltinByteString -> [BuiltinByteString]
shrinkHash hash = do
  toBuiltin . BS.pack . take (BS.length bs) . iterate' (`rotateL` 1)
    <$> shrink seed
  where
    bs = fromBuiltin hash
    seed = BS.head $ fromBuiltin hash

deriving via (ArbitraryHash 32) instance Arbitrary CertificateHash
deriving via (ArbitraryHash 32) instance Arbitrary PubKeyHash
deriving via (ArbitraryHash 32) instance Arbitrary TxId
deriving via (ArbitraryHash 32) instance Arbitrary DatumHash
deriving via (ArbitraryHash 28) instance Arbitrary ScriptHash
deriving via (ArbitraryHash 28) instance Arbitrary CurrencySymbol

instance Arbitrary Credential where
  arbitrary =
    oneof
      [ ScriptCredential <$> arbitrary
      , PubKeyCredential <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary TokenName where
  arbitrary = TokenName . toBuiltin . BS.pack . take 32 <$> arbitrary
  shrink = fmap (TokenName . toBuiltin) . shrink @ByteString . fromBuiltin . unTokenName

instance Arbitrary HotCommitteeCredential where
  arbitrary = HotCommitteeCredential <$> arbitrary
  shrink = genericShrink

instance Arbitrary ColdCommitteeCredential where
  arbitrary = ColdCommitteeCredential <$> arbitrary
  shrink = genericShrink

instance Arbitrary GovernanceActionId where
  arbitrary = GovernanceActionId <$> arbitrary <*> chooseIntegerHyperbolic
  shrink = genericShrink

instance Arbitrary Vote where
  arbitrary = elements [VoteYes, VoteNo, Abstain]

instance Arbitrary Voter where
  arbitrary =
    oneof
      [ DRepVoter <$> arbitrary
      , StakePoolVoter <$> arbitrary
      , CommitteeVoter <$> arbitrary
      ]
  shrink = genericShrink

deriving instance Ord DRepCredential
deriving instance Ord HotCommitteeCredential
deriving instance Ord Voter
deriving instance Ord GovernanceActionId

chooseIntegerHyperbolic :: Gen Integer
chooseIntegerHyperbolic = sized \size -> do
  ξ <- choose (0, 0.99 :: Double)
  pure $ floor $ fromIntegral size * atanh ξ

instance Arbitrary Identity where
  arbitrary = Identity <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ColdLockDatum where
  arbitrary = ColdLockDatum <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ColdLockRedeemer where
  arbitrary =
    oneof
      [ AuthorizeHot <$> arbitrary
      , pure ResignCold
      , ResignDelegation <$> arbitrary
      , pure RotateCold
      , pure BurnCold
      , UpgradeCold <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary HotLockDatum where
  arbitrary = HotLockDatum <$> arbitrary
  shrink = genericShrink

instance Arbitrary HotLockRedeemer where
  arbitrary =
    oneof
      [ pure Vote
      , ResignVoting <$> arbitrary
      , pure RotateHot
      , pure BurnHot
      , UpgradeHot <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary BuiltinData where
  arbitrary = dataToBuiltinData <$> arbitrary
  shrink = fmap dataToBuiltinData . shrink . builtinDataToData

instance Arbitrary Data where
  arbitrary = sized \size -> case size of
    0 -> oneof leaves
    _ -> oneof $ leaves <> nodes size
    where
      leaves = [I <$> arbitrary, B <$> arbitrary]
      nodes size =
        [ do
            len <- chooseInt (0, size)
            let childSize
                  | len == 0 = 0
                  | otherwise = size `div` len
            Constr <$> arbitrary <*> replicateM len (resize childSize arbitrary)
        , do
            len <- chooseInt (0, size)
            let childSize
                  | len == 0 = 0
                  | otherwise = size `div` (len * 2)
            Map <$> replicateM len (resize childSize arbitrary)
        , do
            len <- chooseInt (0, size)
            let childSize
                  | len == 0 = 0
                  | otherwise = size `div` len
            List <$> replicateM len (resize childSize arbitrary)
        ]
  shrink = genericShrink

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> chooseIntegerHyperbolic
  shrink = genericShrink

instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

genTxOut :: Gen Value -> Gen TxOut
genTxOut value = TxOut <$> arbitrary <*> value <*> arbitrary <*> arbitrary

instance Arbitrary Value where
  arbitrary = do
    ada <- arbitrary
    tokens <-
      mapOf arbitrary $
        mapOf arbitrary $
          succ <$> chooseIntegerHyperbolic
    pure $ mkValue ada tokens
  shrink v = do
    let (ada, tokens) = splitValue v
    fold
      [ mkValue <$> shrink ada <*> pure tokens
      , mkValue ada <$> shrink tokens
      ]

mkValue
  :: Lovelace -> Map.Map CurrencySymbol (Map.Map TokenName Integer) -> Value
mkValue (Lovelace ada) tokens =
  Value $
    fromMap $
      fromMap <$> Map.insert adaSymbol (Map.singleton adaToken ada) tokens

splitValue
  :: Value -> (Lovelace, Map.Map CurrencySymbol (Map.Map TokenName Integer))
splitValue (Value (fmap toMap . toMap -> m)) =
  ( Lovelace $ getSum $ (foldMap . foldMap) Sum $ Map.lookup adaSymbol m
  , Map.delete adaSymbol m
  )

mapOf :: (Ord k) => Gen k -> Gen v -> Gen (Map.Map k v)
mapOf k v = Map.fromList <$> listOf ((,) <$> k <*> v)

instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary StakingCredential where
  arbitrary =
    frequency
      [ (9, StakingHash <$> arbitrary)
      ,
        ( 1
        , StakingPtr
            <$> chooseIntegerHyperbolic
            <*> chooseIntegerHyperbolic
            <*> chooseIntegerHyperbolic
        )
      ]
  shrink = genericShrink

deriving newtype instance Arbitrary Datum
deriving newtype instance Arbitrary DRepCredential

instance Arbitrary OutputDatum where
  arbitrary =
    oneof
      [ OutputDatum <$> arbitrary
      , OutputDatumHash <$> arbitrary
      , pure NoOutputDatum
      ]
  shrink = genericShrink

instance Arbitrary CC.ScriptContext where
  arbitrary = CC.ScriptContext <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary CC.ScriptPurpose where
  arbitrary =
    oneof
      [ CC.Minting <$> arbitrary
      , CC.Spending <$> arbitrary
      , CC.Rewarding <$> arbitrary
      , CC.Certifying <$> arbitrary <*> arbitrary
      , CC.Voting <$> arbitrary
      , CC.Proposing <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary CC.TxInfo where
  arbitrary =
    CC.TxInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary CC.TxInInfo where
  arbitrary = CC.TxInInfo <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary HC.ScriptContext where
  arbitrary = HC.ScriptContext <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary HC.ScriptPurpose where
  arbitrary =
    oneof
      [ HC.Minting <$> arbitrary
      , HC.Spending <$> arbitrary
      , HC.Rewarding <$> arbitrary
      , HC.Certifying <$> arbitrary <*> arbitrary
      , HC.Voting <$> arbitrary
      , HC.Proposing <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary HC.TxInfo where
  arbitrary =
    HC.TxInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary HC.TxInInfo where
  arbitrary = HC.TxInInfo <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary CN.ScriptContext where
  arbitrary = do
    purpose <- arbitrary
    CN.ScriptContext <$> genColdNFTTxInfo purpose <*> pure purpose
  shrink ctx@CN.ScriptContext{..} = case scriptContextPurpose of
    CN.Spending ref ->
      fold
        [ shrinkColdSpendingRefPreservingInput ref scriptContextTxInfo
        , shrinkColdInfoPreservingRef ref scriptContextTxInfo
        ]
    _ -> genericShrink ctx

shrinkColdSpendingRefPreservingInput
  :: TxOutRef -> CN.TxInfo -> [CN.ScriptContext]
shrinkColdSpendingRefPreservingInput ref CN.TxInfo{..} = do
  ref' <- shrink ref
  pure
    CN.ScriptContext
      { scriptContextTxInfo =
          CN.TxInfo
            { txInfoInputs = do
                inInfo@TxInInfo{..} <- txInfoInputs
                pure
                  if txInInfoOutRef == ref
                    then TxInInfo ref' txInInfoResolved
                    else inInfo
            , ..
            }
      , scriptContextPurpose = CN.Spending ref'
      }

shrinkColdInfoPreservingRef :: TxOutRef -> CN.TxInfo -> [CN.ScriptContext]
shrinkColdInfoPreservingRef ref info = do
  info' <-
    filter (any ((== ref) . txInInfoOutRef) . CN.txInfoInputs) $ shrink info
  pure $ CN.ScriptContext info' $ CN.Spending ref

genColdNFTTxInfo :: CN.ScriptPurpose -> Gen CN.TxInfo
genColdNFTTxInfo = \case
  CN.Spending ref -> do
    info@CN.TxInfo{..} <- arbitrary
    if any ((== ref) . txInInfoOutRef) txInfoInputs
      then pure info
      else do
        input <- TxInInfo ref <$> arbitrary
        index <- choose (0, length txInfoInputs)
        pure
          CN.TxInfo
            { txInfoInputs = insertAt index input txInfoInputs
            , ..
            }
  _ -> arbitrary

insertAt :: Int -> a -> [a] -> [a]
insertAt _ a [] = [a]
insertAt 0 a as = a : as
insertAt n a (a' : as) = a' : insertAt (pred n) a as

instance Arbitrary CN.ScriptPurpose where
  arbitrary =
    oneof
      [ CN.Minting <$> arbitrary
      , CN.Spending <$> arbitrary
      , CN.Rewarding <$> arbitrary
      , CN.Certifying <$> arbitrary <*> arbitrary
      , CN.Voting <$> arbitrary
      , CN.Proposing <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary CN.TxInfo where
  arbitrary = do
    CN.TxInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

fromMap :: Map.Map k v -> AMap.Map k v
fromMap = AMap.unsafeFromList . Map.toList

toMap :: (Ord k) => AMap.Map k v -> Map.Map k v
toMap = Map.fromList . AMap.toList

chooseTokens
  :: CurrencySymbol
  -> Map.Map TokenName Integer
  -> Gen (Maybe (Map.Map TokenName Integer))
chooseTokens _ supply = do
  chosen <-
    Map.traverseMaybeWithKey (const $ fmap positive . choose . (0,)) supply
  pure do
    guard $ not $ Map.null chosen
    pure chosen

positive :: Integer -> Maybe Integer
positive n
  | n < 1 = Nothing
  | otherwise = Just n

nonzero :: Integer -> Maybe Integer
nonzero n
  | n == 0 = Nothing
  | otherwise = Just n

subtractTokens
  :: Map.Map TokenName Integer
  -> Map.Map TokenName Integer
  -> Maybe (Map.Map TokenName Integer)
subtractTokens = fmap nonempty . Map.differenceWith (fmap nonzero . (-))

nonempty :: Map.Map k a -> Maybe (Map.Map k a)
nonempty m
  | Map.null m = Nothing
  | otherwise = Just m

instance Arbitrary Lovelace where
  arbitrary = Lovelace . max 1 . abs <$> arbitrary
  shrink = mapMaybe (fmap Lovelace . positive . getLovelace) . genericShrink

instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary
  shrink = genericShrink

genTxInInfo :: Gen Value -> Gen TxInInfo
genTxInInfo value = TxInInfo <$> arbitrary <*> genTxOut value

instance Arbitrary DRep where
  arbitrary =
    oneof
      [ DRep <$> arbitrary
      , pure DRepAlwaysAbstain
      , pure DRepAlwaysNoConfidence
      ]
  shrink = genericShrink

instance Arbitrary Delegatee where
  arbitrary =
    oneof
      [ DelegStake <$> arbitrary
      , DelegVote <$> arbitrary
      , DelegStakeVote <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary TxCert where
  arbitrary =
    oneof
      [ TxCertRegStaking <$> arbitrary <*> arbitrary
      , TxCertUnRegStaking <$> arbitrary <*> arbitrary
      , TxCertRegDRep <$> arbitrary <*> arbitrary
      , TxCertUnRegDRep <$> arbitrary <*> arbitrary
      , TxCertRegDeleg <$> arbitrary <*> arbitrary <*> arbitrary
      , TxCertAuthHotCommittee <$> arbitrary <*> arbitrary
      , TxCertDelegStaking <$> arbitrary <*> arbitrary
      , TxCertPoolRegister <$> arbitrary <*> arbitrary
      , TxCertPoolRetire <$> arbitrary <*> arbitrary
      , TxCertUpdateDRep <$> arbitrary
      , TxCertResignColdCommittee <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary HN.ScriptContext where
  arbitrary = do
    purpose <- arbitrary
    HN.ScriptContext <$> genHotNFTTxInfo purpose <*> pure purpose
  shrink = genericShrink

genHotNFTTxInfo :: HN.ScriptPurpose -> Gen HN.TxInfo
genHotNFTTxInfo = \case
  HN.Spending ref -> do
    info@HN.TxInfo{..} <- arbitrary
    if any ((== ref) . txInInfoOutRef) txInfoInputs
      then pure info
      else do
        input <- TxInInfo ref <$> arbitrary
        index <- choose (0, length txInfoInputs)
        pure
          HN.TxInfo
            { txInfoInputs = insertAt index input txInfoInputs
            , ..
            }
  _ -> arbitrary

instance Arbitrary HN.ScriptPurpose where
  arbitrary =
    oneof
      [ HN.Minting <$> arbitrary
      , HN.Spending <$> arbitrary
      , HN.Rewarding <$> arbitrary
      , HN.Certifying <$> arbitrary <*> arbitrary
      , HN.Voting <$> arbitrary
      , HN.Proposing <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary HN.TxInfo where
  arbitrary =
    HN.TxInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (AMap.Map k v) where
  arbitrary = fromMap <$> arbitrary
  shrink = fmap fromMap . shrink . Map.fromList . AMap.toList

genMinoritySignatures :: [Identity] -> Gen [PubKeyHash]
genMinoritySignatures required = go 0 =<< listOf (pure ())
  where
    requiredSigs = pubKeyHash <$> required
    maxHits = (length requiredSigs `div` 2) - 1
    go _ [] = pure []
    go hits (_ : xs) = do
      sig <-
        if hits > maxHits
          then arbitrary `suchThat` (not . (`elem` requiredSigs))
          else arbitrary
      let hits'
            | sig `elem` requiredSigs = succ hits
            | otherwise = hits
      (sig :) <$> go hits' xs

newtype Fraction = Fraction Float
  deriving (Show, Eq, Generic)

instance Arbitrary Fraction where
  arbitrary = Fraction <$> choose (0, 1)
  shrink = genericShrink
