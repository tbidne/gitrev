-- | Internal OsString utilities. Exists primarily to provide lenient
-- encodings (for error reporting).
module Development.GitRev.Internal.OsString
  ( -- * Encoding

    -- ** Total
    encodeLenient,

    -- ** Partial
    encodeThrowM,

    -- * Decoding

    -- ** Total
    decodeLenient,

    -- ** Partial
    decodeThrowM,
  )
where

import Control.Category ((>>>))
import Control.Monad.Catch (MonadThrow (throwM))
import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import System.IO (TextEncoding)
import System.IO qualified as IO
import System.OsPath.Encoding (EncodingException)
import System.OsString (OsString)
import System.OsString qualified as OsString

-- | Partial decoding. Throws 'EncodingException'.
decodeThrowM :: (MonadThrow m) => OsString -> m FilePath
decodeThrowM =
  decode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Partial encoding. Throws 'EncodingException'.
encodeThrowM :: (MonadThrow m) => FilePath -> m OsString
encodeThrowM =
  encode >>> \case
    Right txt -> pure txt
    Left ex -> throwM ex

decode :: OsString -> Either EncodingException FilePath
decode = OsString.decodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

encode :: FilePath -> Either EncodingException OsString
encode = OsString.encodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

-- | Total decoding, replacing errors with the closest visual match.
decodeLenient :: OsString -> FilePath
decodeLenient = elimEx . OsString.decodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | Total encoding, replacing errors with the closest visual match.
encodeLenient :: FilePath -> OsString
encodeLenient = elimEx . OsString.encodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | Normal encodings.
utfEncodings :: (TextEncoding, TextEncoding)
utfEncodings = (IO.utf8, IO.utf16le)

-- | Total encodings, replacing failures with the closest visual match.
utfEncodingsLenient ::
  ( TextEncoding,
    TextEncoding,
    Either EncodingException a -> a
  )
utfEncodingsLenient =
  ( UTF8.mkUTF8 TransliterateCodingFailure,
    UTF16.mkUTF16le TransliterateCodingFailure,
    elimEx
  )
  where
    elimEx = either (error . show) id
