module TxSummary.Error where

import Cardano.Api (FileError (..), TextEnvelopeCddlError (..))
import Control.Exception (Exception (..))
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Formatting.Buildable (Buildable (..))
import TxSummary.Common (ItemM, describe)

renderError :: FileError TextEnvelopeCddlError -> ItemM ()
renderError = \case
  FileError _ (TextEnvelopeCddlErrUnknownType t) ->
    describe $ "Text envelope \"type\" field invalid. Unknown type: " <> t
  FileError _ (TextEnvelopeCddlTypeError expected received) -> do
    describe "Text envelope \"type\" field invalid. Expected one of:"
    traverse_ (describe . ("⋅ " <>)) expected
    describe $ "But got " <> received
  FileError _ (TextEnvelopeCddlErrCBORDecodingError err) -> do
    describe "Text envelope file contains invalid tx body binary data. Details:"
    traverse_ describe $ T.lines (TL.toStrict $ TLB.toLazyText $ build err)
  FileError _ (TextEnvelopeCddlAesonDecodeError _ err) -> do
    describe "Text envelope file contains invalid JSON. Details:"
    describe $ T.pack err
  FileError _ TextEnvelopeCddlUnknownKeyWitness -> pure () -- won't happen here
  FileError _ TextEnvelopeCddlErrByronKeyWitnessUnsupported -> pure () -- won't happen here
  FileErrorTempFile{} -> describe "Unable to open temporary file"
  FileDoesNotExistError{} -> describe "File does not exist"
  FileIOError _ ioErr -> do
    describe "Error reading file:"
    describe $ T.pack $ displayException ioErr
