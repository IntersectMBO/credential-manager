module OrchestratorCLI.TestCommon.CertHierarchy where

import qualified Data.Text as T
import System.FilePath ((</>), takeBaseName)
import Data.Text (Text)
import Control.Applicative (empty)
import OrchestratorCLI.TestCommon.HSpecExtra (shouldCreateFiles, fileShouldExist)
import Turtle (procStrictWithErr, mkdir, void)
import Data.Function ((&))
import qualified Turtle.Bytes as Bytes

type RoleName = Text

newtype BaseRoleDirectory = BaseRoleDirectory String

vkeyFileName :: FilePath
vkeyFileName = "vkey.envelope"

skeyFileName :: FilePath
skeyFileName = "skey.envelope"

-- Blake2b_224 key hash
keyHashFileName :: FilePath
keyHashFileName = "keyhash"

privPemFileName :: FilePath
privPemFileName = "priv.pem"

pubPemFileName :: FilePath
pubPemFileName = "pub.pem"

certReqFileName :: FilePath
certReqFileName = "csr"

certFileName :: FilePath
certFileName = "cert.pem"

certHashFileName :: FilePath
certHashFileName = "certhash"

createRoleDirectory :: RoleName -> FilePath -> IO BaseRoleDirectory
createRoleDirectory roleName rootDir = do
  let roleDir = rootDir </> T.unpack roleName
      vkeyFile = roleDir </> vkeyFileName
      skeyFile = roleDir </> skeyFileName
      privPemFile = roleDir </> privPemFileName
      pubPemFile = roleDir </> pubPemFileName
      keyHashFile = roleDir </> keyHashFileName
  mkdir roleDir

  let
    args = ["address",  "key-gen", "--signing-key-file", T.pack skeyFile, "--verification-key-file", T.pack vkeyFile]
    createKeys = procStrictWithErr "cardano-cli" args empty
  shouldCreateFiles createKeys [skeyFile, vkeyFile]

  let
    convertToPem = procStrictWithErr "orchestrator-cli" ["to-pem", T.pack skeyFile, T.pack privPemFile] empty
  shouldCreateFiles convertToPem [privPemFile]

  let
    convertToPubPem = procStrictWithErr "openssl" ["pkey", "-in", T.pack privPemFile, "-pubout", "-out", T.pack pubPemFile] empty
  shouldCreateFiles convertToPubPem [pubPemFile]

  let
    keyHashArgs = ["address", "key-hash", "--payment-verification-key-file", T.pack vkeyFile, "--out-file", T.pack keyHashFile]
    createKeyHash = procStrictWithErr "cardano-cli" keyHashArgs empty
  shouldCreateFiles createKeyHash [keyHashFile]

  return $ BaseRoleDirectory roleDir

newtype RootRoleDirectory = RootRoleDirectory String

-- Add self signed ceriticate
selfCertifyRoot :: BaseRoleDirectory -> IO RootRoleDirectory
selfCertifyRoot (BaseRoleDirectory baseDir) = do
  let
    roleName = do
      let base = takeBaseName baseDir
      if base == "" then "root" else base
    privPemFile = baseDir </> "priv.pem"
    certPemFile = baseDir </> "cert.pem"
    args =
      [ "req", "-x509", "-new", "-nodes", "-key", T.pack privPemFile
      , "-sha256", "-days", "3650" , "-out", T.pack certPemFile
      , "-subj", "/C=NL/ST=Utrecht/L=Utrecht/O=EXAMPLE_ORG/OU=Core Tech/CN=CA/emailAddress=" <> T.pack roleName <> "@example.com"
      ]
  shouldCreateFiles (procStrictWithErr "openssl" args empty) [certPemFile]

  let
    certHashFile = baseDir </> certHashFileName
  void
    $ Bytes.inproc "openssl" ["x509", "-in", T.pack certPemFile, "-pubkey"] empty
    & Bytes.inproc "sha256sum" []
    & Bytes.inproc "head" ["-c", "64"]
    & Bytes.output certHashFile
  fileShouldExist certHashFile
  return $ RootRoleDirectory baseDir

newtype ChildRoleDirectory = ChildRoleDirectory String

-- Add cert file to existing role directory
certifyChild :: RootRoleDirectory -> BaseRoleDirectory -> IO ChildRoleDirectory
certifyChild (RootRoleDirectory rootDir) (BaseRoleDirectory childDir) = do
  let
    roleName = do
      let base = takeBaseName childDir
      if base == "" then "child" else base
    childPrivPemFile = childDir </> "priv.pem"
    childCertPemFile = childDir </> "cert.pem"
    childCertReqFile = childDir </> "csr"
    reqArgs =
      [ "req", "-new", "-key", T.pack childPrivPemFile, "-out", T.pack childCertReqFile
      , "-subj", "/C=NL/ST=Utrecht/L=Utrecht/O=EXAMPLE_ORG/OU=Core Tech/CN=Child " <> T.pack roleName <> "/emailAddress=" <> T.pack roleName <> "@example.com"
      ]
  shouldCreateFiles (procStrictWithErr "openssl" reqArgs empty) [childCertReqFile]

  let
    certArgs =
      [ "x509", "-req", "-in", T.pack childCertReqFile, "-CA", T.pack (rootDir </> "cert.pem"), "-CAkey", T.pack (rootDir </> "priv.pem")
      , "-CAcreateserial", "-out", T.pack childCertPemFile, "-days", "365", "-sha256"
      ]
  shouldCreateFiles (procStrictWithErr "openssl" certArgs empty) [childCertPemFile]

  let
    childCertHashFile = childDir </> certHashFileName
  void
    $ Bytes.inproc "openssl" ["x509", "-in", T.pack childCertPemFile, "-pubkey"] empty
    & Bytes.inproc "sha256sum" []
    & Bytes.inproc "head" ["-c", "64"]
    & Bytes.output childCertHashFile
  fileShouldExist childCertHashFile
  return $ ChildRoleDirectory childDir

newtype CertifiedRoleDirectory = CertifiedRoleDirectory String

certifiedFromRootRoleDirectory :: RootRoleDirectory -> CertifiedRoleDirectory
certifiedFromRootRoleDirectory (RootRoleDirectory rootDir) = CertifiedRoleDirectory rootDir

certifiedFromChildRoleDirectory :: ChildRoleDirectory -> CertifiedRoleDirectory
certifiedFromChildRoleDirectory (ChildRoleDirectory childDir) = CertifiedRoleDirectory childDir

roleCertFile :: CertifiedRoleDirectory -> FilePath
roleCertFile (CertifiedRoleDirectory roleDir) = roleDir </> certFileName

roleCertHashFile :: CertifiedRoleDirectory -> FilePath
roleCertHashFile (CertifiedRoleDirectory roleDir) = roleDir </> certHashFileName
