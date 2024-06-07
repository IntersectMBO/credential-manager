module TxSummary.Error where

import Cardano.Api (FileError (..), TextEnvelopeCddlError (..))
import Control.Exception (Exception (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Formatting.Buildable (Buildable (..))

renderError :: FileError TextEnvelopeCddlError -> [Text]
renderError = \case
  FileError _ (TextEnvelopeCddlErrUnknownType t) ->
    ["Text envelope \"type\" field invalid. Unknown type: " <> t]
  FileError _ (TextEnvelopeCddlTypeError expected received) ->
    concat
      [ pure "Text envelope \"type\" field invalid. Expected one of:"
      , fmap ("⋅ " <>) expected
      , pure $ "But got " <> received
      ]
  FileError _ (TextEnvelopeCddlErrCBORDecodingError err) ->
    "Text envelope file contains invalid tx body binary data. Details:"
      : T.lines (TL.toStrict $ TLB.toLazyText $ build err)
  FileError _ (TextEnvelopeCddlAesonDecodeError _ err) ->
    [ "Text envelope file contains invalid JSON. Details:"
    , T.pack err
    ]
  FileError _ TextEnvelopeCddlUnknownKeyWitness -> [] -- won't happen here
  FileError _ TextEnvelopeCddlErrByronKeyWitnessUnsupported -> [] -- won't happen here
  FileErrorTempFile{} -> ["Unable to open temporary file"]
  FileDoesNotExistError{} -> ["File does not exist"]
  FileIOError _ ioErr ->
    [ "Error reading file:"
    , T.pack $ displayException ioErr
    ]
