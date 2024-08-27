{-# LANGUAGE OverloadedStrings #-}

module OrchestratorCLI.GuideSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import OrchestratorCLI.TestCommon (withOrchestartorEnv, runOrchestratorCli, shouldCreateFiles)
import OrchestratorCLI.TestCommon.Orchestrator (getOrchestratorUTxO, TxOutRef (TxOutRef), Orchestrator (..), mkEnterpriseAddress, EnterpriseAddress (..), Wallet (Wallet, paymentKeyPair))
import qualified Data.Text as T
import OrchestratorCLI.TestCommon.CertHierarchy (RootRoleDirectory, ChildRoleDirectory, createRoleDirectory, selfCertifyRoot, certifyChild, certifiedFromRootRoleDirectory, roleCertFile, certifiedFromChildRoleDirectory, roleCertHashFile)
import Control.Monad (void)
import System.FilePath ((</>))
import OrchestratorCLI.TestCommon.HSpecExtra (shouldExecuteSuccessfully, catFile)
import Control.Applicative (Alternative(empty))
import Turtle.Prelude (procStrictWithErr)
import OrchestratorCLI.TestCommon.Testnet (PaymentKeyPair(PaymentKeyPair, sKeyFile))
import Turtle (unsafeTextToLine)
import Turtle.Prelude (echo)

data MinimalCertTree = MinimalCertTree
  { caDir :: RootRoleDirectory
  , memberDir :: ChildRoleDirectory
  , delegatorDir :: ChildRoleDirectory
  , voterDir :: ChildRoleDirectory
  }

mkMinimalCertTree :: FilePath -> IO MinimalCertTree
mkMinimalCertTree tempDir = do
  caDir <- createRoleDirectory "ca" tempDir >>= selfCertifyRoot
  member1Dir <- createRoleDirectory "membership1" tempDir >>= certifyChild caDir
  delegator1Dir <- createRoleDirectory "delegation1" tempDir >>= certifyChild caDir
  voter1Dir <- createRoleDirectory "voter1" tempDir >>= certifyChild caDir
  return $ MinimalCertTree caDir member1Dir delegator1Dir voter1Dir

--         $ diff \
--            <(cat example-certificates/ca-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[0].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-1/child-1.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[0].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-1/child-1-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[0].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-2/child-2.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[1].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-2/child-2-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[1].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-3/child-3.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[2].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-3/child-3-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[1].list[2].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-4/child-4.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[0].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-4/child-4-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[0].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-5/child-5.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[1].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-5/child-5-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[1].fields[1].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-6/child-6.keyhash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[2].fields[0].bytes')
--         $ diff \
--            <(cat example-certificates/children/child-6/child-6-cert.hash) \
--            <(cat init-cold/nft.datum.json | jq -r '.fields[2].list[2].fields[1].bytes')
              


spec :: Spec
spec = withOrchestartorEnv $
  describe "Orchestrator CLI Guide" $ do
      it "Initializes cold credentials correctly" \(_, tempDir) -> do
        MinimalCertTree { caDir, memberDir, delegatorDir } <- mkMinimalCertTree tempDir
        let
            caPem = roleCertFile $ certifiedFromRootRoleDirectory caDir
            memberPem = roleCertFile $ certifiedFromChildRoleDirectory memberDir
            delegatorPem = roleCertFile $ certifiedFromChildRoleDirectory delegatorDir

        let args = [ "--testnet"
                   , "-o", T.pack tempDir
                   , "--seed-input", "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef#1"
                   , "--ca-cert", T.pack caPem
                   , "--membership-cert", T.pack memberPem
                   , "--delegation-cert", T.pack delegatorPem
                   ]
        shouldCreateFiles
          ( runOrchestratorCli
            "init-cold"
            args
          )
          $ fmap (tempDir </>)
           [ "credential.plutus"
           , "credential.plutus.hash"
           , "minting.plutus"
           , "minting.plutus.hash"
           , "mint.redeemer.json"
           , "nft-token-name"
           , "nft.datum.json"
           ]

        let
          hashFile = roleCertHashFile $ certifiedFromRootRoleDirectory caDir
        certHash <- catFile hashFile
        jsonContent <- catFile $ tempDir </> "nft.datum.json"
        echo $ unsafeTextToLine jsonContent
        datumCertHash <- shouldExecuteSuccessfully
            ( procStrictWithErr
                "jq"
                ["-r", ".fields[0].fields[1].bytes", T.pack $ tempDir </> "nft.datum.json"]
                empty
            )
        datumCertHash `shouldBe` certHash
        --  "bytes": "66889602a42a0ee9f93492ff73230b6c5c8ce6396df8469fe8c21739"
        --  "bytes": "4950ac8f4a78b8823b8293c588c132f2ed510aed776c9a47042c9c53becd3966"


      -- We can not e2e test this flow becuase CC credentials are hard coded for the static
      -- NFT flow. At least we can check the minting results.
      it "Step 2a - Creating the assets" \(orchestrator, tempDir) -> do
        MinimalCertTree { caDir, memberDir, delegatorDir } <- mkMinimalCertTree tempDir
        TxOutRef txOutRef <- getOrchestratorUTxO orchestrator
        let
          caPem = roleCertFile $ certifiedFromRootRoleDirectory caDir
          memberPem = roleCertFile $ certifiedFromChildRoleDirectory memberDir
          delegatorPem = roleCertFile $ certifiedFromChildRoleDirectory delegatorDir

          args =
            [ "--seed-input", txOutRef
            , "--ca-cert", T.pack caPem
            , "--membership-cert", T.pack memberPem
            , "--delegation-cert", T.pack delegatorPem
            , "-o", T.pack tempDir
            , "--testnet"
            ]
        void $ shouldExecuteSuccessfully $ runOrchestratorCli "init-cold" args

        let
          Orchestrator wallet@Wallet { paymentKeyPair = PaymentKeyPair { sKeyFile }} = orchestrator
        EnterpriseAddress orchestratorAddr <- mkEnterpriseAddress wallet
        nftAddr <- catFile $ tempDir </> "nft.addr"
        mintingPlutusHash <- catFile $ tempDir </> "minting.plutus.hash"
        nftTokenName <- catFile $ tempDir </> "nft-token-name"

        let
          bodyFile = T.pack $ tempDir </> "body.json"
          txFile = T.pack $ tempDir </> "tx.json"

        void $ shouldExecuteSuccessfully $
          procStrictWithErr
          "cardano-cli"
          [ "conway", "transaction", "build"
          , "--change-address", orchestratorAddr
          , "--tx-in", txOutRef
          , "--tx-in-collateral", txOutRef
          , "--tx-out", nftAddr <> " + 5000000 + 1 " <> mintingPlutusHash <> "." <> nftTokenName
          , "--tx-out-inline-datum-file", T.pack $ tempDir </> "nft.datum.json"
          , "--mint", "1 " <> mintingPlutusHash <> "." <> nftTokenName
          , "--mint-script-file", T.pack $ tempDir </> "minting.plutus"
          , "--mint-redeemer-file", T.pack $ tempDir </> "mint.redeemer.json"
          , "--out-file", bodyFile
          ]
          empty

        void $ shouldExecuteSuccessfully $
          procStrictWithErr
          "cardano-cli"
          [ "conway", "transaction", "sign"
          , "--signing-key-file", T.pack sKeyFile
          , "--tx-body-file", bodyFile
          , "--out-file", txFile
          ]
          empty

        void $ shouldExecuteSuccessfully $
          procStrictWithErr
          "cardano-cli"
          [ "conway", "transaction", "submit"
          , "--tx-file", txFile
          ]
          empty



-- getSeedInput :: IO T.Text
-- getSeedInput = do
--   (exitCode, stdout, _) <- procStrictWithErr "cardano-cli" 
--     [ "query", "utxo"
--     , "--testnet-magic", "42"
--     , "--out-file", "/dev/stdout"
--     ] empty
--   case exitCode of
--     ExitSuccess -> do
--       let utxos = T.lines stdout
--       case utxos of
--         (_:utxo:_) -> return $ T.takeWhile (/= ' ') utxo  -- Take the first UTXO
--         _ -> error "No UTXOs found"
--     ExitFailure _ -> error "Failed to query UTXOs"
-- 
-- initializeColdCredential :: FilePath -> IO ()
-- initializeColdCredential tempDir = do
--   caDir <- createRoleDirectory "ca" tempDir >>= selfCertifyRoot
--   membership1Dir <- createRoleDirectory "membership1" tempDir >>= certifyChild caDir
--   delegation1Dir <- createRoleDirectory "delegation1" tempDir >>= certifyChild caDir
-- 
--   let caPem = roleCertFile $ certifiedFromRootRoleDirectory caDir
--       membership1Pem = roleCertFile $ certifiedFromChildRoleDirectory membership1Dir
--       delegation1Pem = roleCertFile $ certifiedFromChildRoleDirectory delegation1Dir
-- 
--   seedInput <- getSeedInput
-- 
--   let args = [ "init-cold"
--              , "--testnet"
--              , "--seed-input", seedInput
--              , "--ca-cert", T.pack caPem
--              , "--membership-cert", T.pack membership1Pem
--              , "--delegation-cert", T.pack delegation1Pem
--              , "-o", T.pack tempDir
--              ]
-- 
--   result <- try $ runOrchestratorCli "init-cold" (tail args)
--   result `shouldSatisfy` isRight
-- 
-- initializeHotCredential :: FilePath -> IO ()
-- initializeHotCredential tempDir = do
--   -- We'll implement this in the next step
--   touch (tempDir </> "hot_credential_initialized")
