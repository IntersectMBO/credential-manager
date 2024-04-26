{-# OPTIONS_GHC -Wno-orphans #-}

module CredentialManager.Gen where

import Control.Monad (replicateM, (<=<))
import CredentialManager.Api
import qualified CredentialManager.Scripts.ColdCommittee as CC
import qualified CredentialManager.Scripts.ColdNFT as CN
import qualified CredentialManager.Scripts.HotCommittee as HC
import qualified CredentialManager.Scripts.HotNFT as HN
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Proxy (..))
import Data.List (iterate')
import qualified Data.Map as Map
import Data.Word (Word8)
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
  arbitrary = GovernanceActionId <$> arbitrary <*> chooseIntegerExponential
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

chooseIntegerExponential :: Gen Integer
chooseIntegerExponential = sized \case
  0 -> pure 0
  size -> do
    let λ = 1 / fromIntegral size
    ξ <- choose (0, -0.999999 :: Double)
    pure $ floor $ negate $ log (1 - ξ) / λ

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
      , pure Unlock
      ]
  shrink = genericShrink

instance Arbitrary HotLockDatum where
  arbitrary = HotLockDatum <$> arbitrary
  shrink = genericShrink

instance Arbitrary HotLockRedeemer where
  arbitrary =
    oneof
      [ Vote <$> arbitrary <*> arbitrary
      , ResignVoting <$> arbitrary
      , pure RotateHot
      , pure BurnHot
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
  arbitrary = TxOutRef <$> arbitrary <*> chooseIntegerExponential
  shrink = genericShrink

instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Value where
  arbitrary = do
    ada <- abs <$> arbitrary
    tokens <-
      listOf $
        (,)
          <$> arbitrary
          <*> listOf ((,) <$> arbitrary <*> chooseIntegerExponential)
    let valueMap =
          Map.insert "" (Map.singleton "" ada) $
            Map.fromList <$> Map.fromList tokens
    pure $
      Value $
        AMap.fromList $
          Map.toList $
            AMap.fromList . Map.toList <$> valueMap
  shrink =
    fmap
      ( Value
          . AMap.fromList
          . Map.toList
          . fmap (AMap.fromList . Map.toList)
      )
      . shrink
      . fmap (Map.fromList . AMap.toList)
      . Map.fromList
      . AMap.toList
      . getValue

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
            <$> chooseIntegerExponential
            <*> chooseIntegerExponential
            <*> chooseIntegerExponential
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
  arbitrary = CN.ScriptContext <$> arbitrary <*> arbitrary
  shrink = genericShrink

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
  arbitrary =
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

instance Arbitrary Lovelace where
  arbitrary = Lovelace . abs <$> arbitrary
  shrink = genericShrink

instance Arbitrary TxInInfo where
  arbitrary = TxInInfo <$> arbitrary <*> arbitrary
  shrink = genericShrink

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
  arbitrary = HN.ScriptContext <$> arbitrary <*> arbitrary
  shrink = genericShrink

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
  arbitrary = AMap.fromList . Map.toList <$> arbitrary
  shrink = fmap (AMap.fromList . Map.toList) . shrink . Map.fromList . AMap.toList
