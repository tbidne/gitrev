-- | Internal OsString utilities. Exists primarily to provide lenient
-- encodings (for error reporting).
--
-- @since 0.1
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

import Control.Monad.Catch (MonadThrow)
import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.Latin1 qualified as Latin1
import System.IO (TextEncoding)
import System.OsPath.Encoding (EncodingException)
import System.OsString (OsString)
import System.OsString qualified as OsString
import System.OsString.Encoding qualified as Enc

-- | Partial decoding. Throws 'EncodingException'.
--
-- @since 0.1
decodeThrowM :: forall m. (MonadThrow m) => OsString -> m FilePath
decodeThrowM = OsString.decodeUtf

-- | Partial encoding. Throws 'EncodingException'.
--
-- @since 0.1
encodeThrowM :: forall m. (MonadThrow m) => FilePath -> m OsString
encodeThrowM = OsString.encodeUtf

-- | Total decoding, replacing errors with the closest visual match.
-- Latin1 on posix, Ucs2le on windows. This is intended for situations where
-- distortion is preferable to a crash e.g. error rendering.
--
-- @since 0.1
decodeLenient :: OsString -> FilePath
decodeLenient = elimEx . OsString.decodeWith posixEncoding windowsEncoding
  where
    (posixEncoding, windowsEncoding, elimEx) = encodingsLenient

-- | Total encoding, replacing errors with the closest visual match.
-- Latin1 on posix, Ucs2le on windows. This is intended for situations where
-- distortion is preferable to a crash e.g. error rendering.
--
-- @since 0.1
encodeLenient :: FilePath -> OsString
encodeLenient = elimEx . OsString.encodeWith posixEncoding windowsEncoding
  where
    (posixEncoding, windowsEncoding, elimEx) = encodingsLenient

-- Total encodings.
encodingsLenient ::
  forall a.
  ( TextEncoding,
    TextEncoding,
    Either EncodingException a -> a
  )
encodingsLenient =
  ( -- While these __shouldn't__ fail, hence TransliterateCodingFailure is
    -- unnecessary, I'm less sure about ucs2le. Since we really want these to
    -- be total (garbage decodes are regrettable, but we can live with it),
    -- I see no reason not to use TransliterateCodingFailure, out of paranoia.
    Latin1.mkLatin1 TransliterateCodingFailure,
    Enc.mkUcs2le TransliterateCodingFailure,
    elimEx
  )
  where
    elimEx = either (error . show) id
