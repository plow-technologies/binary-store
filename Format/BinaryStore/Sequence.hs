
-- | Format to encode a sequence of binary stores.
module Format.BinaryStore.Sequence (
    BinaryStoreSequence (..)
  , encodeSequence
  , decodeSequence
  ) where

import Format.BinaryStore

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
  ( Builder
  , toLazyByteString
  , lazyByteString
  , word8
  , int64LE
    )
import Data.Binary.Get
  ( Get
  , runGetOrFail
  , getLazyByteString
  , getWord8
  , getInt64le
    )
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.ByteString.Lazy.Char8 (unpack)

data BinaryStoreSequence =
    BS_Node BinaryStore BinaryStoreSequence
  | BS_Error String
  | BS_End

sequenceBuilder :: BinaryStoreSequence -> Builder
sequenceBuilder (BS_Node bs bss) =
  let b = encode bs
  in     word8 0
      <> int64LE (B.length b)
      <> lazyByteString b
      <> sequenceBuilder bss
sequenceBuilder BS_End = word8 1
sequenceBuilder (BS_Error err) =
  let b = fromString err
  in     word8 2
      <> int64LE (B.length b)
      <> lazyByteString b

-- | Encode a binary store sequence as a lazy bytestring.
encodeSequence :: BinaryStoreSequence -> ByteString
encodeSequence = toLazyByteString . sequenceBuilder

data BinaryStoreSequenceIncrement =
    BS_Incomplete (BinaryStoreSequence -> BinaryStoreSequence)
  | BS_Complete BinaryStoreSequence

getBinaryStoreSequenceIncrement :: Get BinaryStoreSequenceIncrement
getBinaryStoreSequenceIncrement = do
  w <- getWord8
  case w of
    0 -> do l <- getInt64le
            b <- getLazyByteString l
            pure $ case decode b of
              Left err -> BS_Complete $ BS_Error err
              Right bs -> BS_Incomplete $ BS_Node bs
    1 -> pure $ BS_Complete BS_End
    2 -> do l <- getInt64le
            b <- getLazyByteString l
            pure $ BS_Complete $ BS_Error $ unpack b
    _ -> fail $ "BinaryStoreSequenceIncrement: invalid byte: " ++ show w

-- | Decode a binary store sequence from a lazy bytestring.
decodeSequence :: ByteString -> BinaryStoreSequence
decodeSequence b =
  case runGetOrFail getBinaryStoreSequenceIncrement b of
    Left (_, _, err) -> BS_Error err
    Right (unconsumed, _, bssi) ->
      case bssi of
        BS_Incomplete f -> f (decodeSequence unconsumed)
        BS_Complete bss -> bss
