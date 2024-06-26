{-# OPTIONS_GHC -Wno-orphans #-}

module CredentialManager.Gen where

import Control.Monad (guard, replicateM, (<=<))
import CredentialManager.Api
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.Foldable (Foldable (..))
import Data.List (iterate')
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass))
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
deriving via (ArbitraryHash 28) instance Arbitrary PubKeyHash
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

instance Arbitrary AssetClass where
  arbitrary = curry AssetClass <$> arbitrary <*> arbitrary
  shrink = genericShrink

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
deriving newtype instance Arbitrary Redeemer
deriving newtype instance Arbitrary DRepCredential

instance Arbitrary OutputDatum where
  arbitrary =
    oneof
      [ OutputDatum <$> arbitrary
      , OutputDatumHash <$> arbitrary
      , pure NoOutputDatum
      ]
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

deriving via (Positive Integer) instance Arbitrary Lovelace
deriving via (NonNegative Integer) instance Arbitrary POSIXTime

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

instance (Arbitrary a) => Arbitrary (Extended a) where
  arbitrary =
    oneof
      [ pure NegInf
      , Finite <$> arbitrary
      , pure PosInf
      ]
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (LowerBound a) where
  arbitrary = LowerBound <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (UpperBound a) where
  arbitrary = UpperBound <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary POSIXTimeRange where
  arbitrary = Interval <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary GovernanceAction where
  arbitrary = pure InfoAction
  shrink = const []

instance Arbitrary ProposalProcedure where
  arbitrary = ProposalProcedure <$> arbitrary <*> arbitrary <*> arbitrary
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

instance Arbitrary ScriptPurpose where
  arbitrary =
    oneof
      [ Minting <$> arbitrary
      , Spending <$> arbitrary
      , Rewarding <$> arbitrary
      , Certifying <$> arbitrary <*> arbitrary
      , Voting <$> arbitrary
      , Proposing <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary ScriptInfo where
  arbitrary =
    oneof
      [ MintingScript <$> arbitrary
      , SpendingScript <$> arbitrary <*> arbitrary
      , RewardingScript <$> arbitrary
      , CertifyingScript <$> arbitrary <*> arbitrary
      , VotingScript <$> arbitrary
      , ProposingScript <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary TxInfo where
  arbitrary =
    TxInfo
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

instance Arbitrary ScriptContext where
  arbitrary = ScriptContext <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink
