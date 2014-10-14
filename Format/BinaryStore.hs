
-- | A /Binary Store/ is a data format that stores a sequence of values
--   encoded using the binary transform. Therefore, it use is restricted
--   to binary lists, i.e. lists whose length is a power of two.
--
--   To create a binary store from a binary list use 'createBinaryStore'.
--   This will create a 'BinaryStore' value that you can convert to a
--   lazy 'ByteString' with 'encode'. To revert the process, use 'decode'
--   and then 'readBinaryStore'. The whole process of decoding, from 'ByteString'
--   to the final 'Decoded' value is done /lazily/. This allows the user to
--   decode only a portion of the data without reading the full 'ByteString'.
--   This is useful when reading big files or when the 'ByteString' is obtained
--   via network connection.
--
module Format.BinaryStore (
      -- * Binary Store Type
      BinaryStore
      -- * Encoding/Decoding
    , encode, decode
      -- * Creating and reading
    , createBinaryStore
    , createBinaryStoreDefault
    , readBinaryStore
      -- * Class of storable values
    , BinaryStoreValue
      -- * Information
      -- | Some functions to get information about a binary store.
    , Mode (..)
    , bsMode
    , bsNumerator
    , bsDenominator
    , averageConstant
    , bsDirection
    , bsCompression
    , bsLength
    , bsData
    ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Arrow ((***))

-- Binary lists
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL
import Data.BinaryList.Serialize (Direction (..),Decoded)
import qualified Data.BinaryList.Serialize as BLS

-- Bytestrings
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as SB

-- Binary
import Data.Word (Word8)
import Data.Binary (Binary (..))
import Data.Binary.Put ( Put,runPut,putWord8,putWord64le
                       , putByteString,putLazyByteString )
import Data.Binary.Get (Get,runGetOrFail,getWord8,getWord64le)

-- Casting from/to Double
import Data.ReinterpretCast (doubleToWord,wordToDouble)

-- Binary Transform
import Data.BinaryList.Algorithm.BinaryTransform

-- Utils

-- | A custom error message for parsing errors in this module.
failGet :: String -> Get a
failGet str = fail $ "binary-store: " ++ str

--------

-- | Binary Store Mode. The Binary Store Mode indicates what
--   kind of data a binary store contains.
--
-- * 'Plain': Each value is a 'Double'.
--
-- * 'WithHoles': Each value is a 'Maybe' 'Double'.
--
data Mode = Plain | WithHoles

-- | Serialization of modes.
putMode :: Mode -> Put
putMode Plain = putWord8 0
putMode WithHoles = putWord8 1

-- | Deserialization of modes.
getMode :: Get Mode
getMode = do
  w <- getWord8
  case w of
    0 -> return Plain
    1 -> return WithHoles
    _ -> failGet $ "unrecognized mode (" ++ show w ++ ")"

-- | Serialization of directions.
putDirection :: Direction -> Put
putDirection FromLeft = putWord8 0
putDirection FromRight = putWord8 1

-- | Deserialization of directions.
getDirection :: Get Direction
getDirection = do
  w <- getWord8
  case w of
    0 -> return FromLeft
    1 -> return FromRight
    _ -> failGet $ "invalid direction (" ++ show w ++ ")"

-- | Binary Store is a format to store data encoded using
--   the Average Binary Transform.
data BinaryStore = BinaryStore {
    bsMode        :: Mode       -- ^ Binary Store mode.
  , bsNumerator   :: Word8      -- ^ Numerator of the Average Constant.
  , bsDenominator :: Word8      -- ^ Denominator of the Average Constant.
  , bsDirection   :: Direction  -- ^ Direction of encoding.
  , bsCompression :: Bool       -- ^ Whether zero compression is used or not.
  , bsLength      :: Word8      -- ^ Length index of the data.
  , bsData        :: ByteString -- ^ Data (which might be compressed).
  }

-- | The constant used by the Average Binary Transform.
averageConstant :: BinaryStore -> Double
averageConstant bs = fromIntegral (bsNumerator bs) / fromIntegral (bsDenominator bs)

-- | Encode a binary store as a lazy 'ByteString'.
encode :: BinaryStore -> ByteString
encode bs = runPut $ do
  putMode $ bsMode bs
  putWord8 $ bsNumerator bs
  putWord8 $ bsDenominator bs
  putDirection $ bsDirection bs
  put $ bsCompression bs -- False encodes as 0. True encodes as 1.
  putWord8 $ bsLength bs
  putLazyByteString $ bsData bs

-- | Decode a lazy 'ByteString' as a binary store. The header of the
--   binary store is read strictly, while the body evaluation is delayed.
--   It returns a 'String' if the header is malformed. The 'String' contains
--   an error description.
decode :: ByteString -> Either String BinaryStore
decode bs = case runGetOrFail getHeader bs of
  Left (_,off,err) -> Left $ err ++ ", after " ++ show off ++ " bytes"
  Right (b,_,(m,n,d,dr,c,l)) -> Right $ BinaryStore m n d dr c l b

-- | Binary Store Header parser.
getHeader :: Get (Mode,Word8,Word8,Direction,Bool,Word8)
getHeader = do
  m <- getMode
  n <- getWord8
  d <- getWord8
  when (d == 0) $ failGet "denominator is zero"
  when (d  < n) $ failGet "denominator is smaller than numerator"
  dr <- getDirection
  c <- get
  l <- getWord8
  return (m,n,d,dr,c,l)

{- Compression algorithm

A simple compression algorithm for bytestrings. If desired, data can be
compressed further using any common algorithm. This algorithm emphasizes
the compression of blocks formed exclusively by zeroes. This is how it
works:

Any sequence of zero bytes is replaced by two bytes. The first byte is
zero. The second byte is the length of the sequence of zeroes. Note that
a byte can only contain a number from 0 to 255. Therefore, the maximum
compression we can get is 255 bytes to 2 bytes. Also note that if a zero
byte is alone, then the compression will be 1 byte to 2 bytes.

-}

-- | Stream zero compression.
putCompressed :: ByteString -> Put
putCompressed = go 0 . B.unpack
  where
    go z (w:ws) =
      case w of
        0 -> case z of
               255 -> do putWord8 0
                         putWord8 255
                         go 1 ws
               _ -> go (z+1) ws
        _ -> case z of
               0 -> do putWord8 w
                       go z ws
               _ -> do putWord8 0
                       putWord8 z
                       putWord8 w
                       go 0 ws
    go z [] =
      case z of
        0 -> return ()
        _ -> do putWord8 0
                putWord8 z

-- | Zero compression of lazy 'ByteString'.
compress :: ByteString -> ByteString
compress = runPut . putCompressed

-- | Zero decompression of lazy 'ByteString'.
decompress :: ByteString -> ByteString
decompress = runPut . go False . B.unpack
  where
    go b (w:ws) =
      if b then do putByteString $ SB.replicate (fromIntegral w) 0
                   go False ws
           else case w of
                  0 -> go True ws
                  _ -> do putWord8 w
                          go False ws
    go _ _ = return ()

----------------------------------
-- Conversion from/to binary lists

-- | This is the class of values that can be stored in a 'BinaryStore'.
--   This is a closed class, so the user is not allowed to add new instances.
class BinaryStoreValue a where
  putValue :: a -> Put
  getValue :: Get a
  modeValue :: a -> Mode
  averageBijection :: Double -> Bijection (a,a) (a,a)

instance BinaryStoreValue Double where
  putValue = putWord64le . doubleToWord
  getValue = wordToDouble <$> getWord64le
  modeValue _ = Plain
  averageBijection p = Bijection f f'
    where
      q = 1 - p
      d = p ^ (2 :: Int) + q ^ (2 :: Int)
      f  (x,y) = (  q*x + p*y    ,  q*y - p*x    )
      f' (x,y) = ( (q*x - p*y)/d , (q*y + p*x)/d )

instance BinaryStoreValue a => BinaryStoreValue (Maybe a) where
  putValue (Just x) = putWord8 1 >> putValue x
  putValue Nothing  = putWord8 0
  getValue = do
    b <- getWord8
    case b of
      0 -> return Nothing
      1 -> Just <$> getValue
      _ -> fail "getValue (Maybe): invalid encoding"
  modeValue _ = WithHoles
  averageBijection p = Bijection f f'
    where
      f  (Just x, Just y) = Just *** Just $ direct  (averageBijection p) (x,y)
      f  v = v
      f' (Just x, Just y) = Just *** Just $ inverse (averageBijection p) (x,y)
      f' v = v

{-# INLINE fromRight #-}

-- | Deconstructor of 'Right'. Throws an error when given a 'Left' value.
fromRight :: Either a b -> b
fromRight e =
  case e of
    Right x -> x
    _ -> error "fromRight: Left value"

{-# INLINE createBinaryStoreDefault #-}

-- | Create a binary store from a binary list, using 'createBinaryStore' with default arguments.
--   In spite of seeming partial (since 'createBinaryStore' returns an 'Either' value), this
--   function is total.
createBinaryStoreDefault :: BinaryStoreValue a => BinList a -> BinaryStore
createBinaryStoreDefault = fromRight . createBinaryStore FromLeft 1 2 True

-- | Create a binary store from a binary list, using some configurations.
--   The denominator of the average constant must be greater or equal to
--   its numerator and, of course, different from zero.
createBinaryStore :: BinaryStoreValue a
                  => Direction -- ^ Direction of encoding
                  -> Word8     -- ^ Average constant numerator
                  -> Word8     -- ^ Average constant denominator
                  -> Bool      -- ^ Whether to use zero compression or not
                  -> BinList a -- ^ Input list
                  -> Either String BinaryStore
createBinaryStore dr n d c xs =
  if d == 0
     then Left "denominator is zero"
     else if d < n
             then Left "denominator is smaller than numerator"
             else Right $ BinaryStore (modeValue $ BL.head xs) n d dr c (fromIntegral $ BL.lengthIndex xs) $
                    let p     = fromIntegral n / fromIntegral d
                        trans = (if dr == FromLeft
                                    then leftBinaryTransform
                                    else rightBinaryTransform) $ averageBijection p
                        comp  = if c then compress else id
                    in  comp $ BLS.encData $ BLS.encodeBinList putValue dr $ direct trans xs

-- | Read a binary store and build a 'Decoded' value. The 'Decoded' value is a list of partial results of
--   increasing size (1, 2, 4, 8, etc) that ends in either a decoding error or a final result. These partial
--   results are generated lazily from the binary store data.
readBinaryStore :: BinaryStoreValue a => BinaryStore -> Decoded a
readBinaryStore bs =
  let decomp  = if bsCompression bs then decompress else id
      encd    = BLS.EncodedBinList (bsDirection bs) (fromIntegral $ bsLength bs) $ decomp $ bsData bs
      p       = averageConstant bs
      detrans = (if bsDirection bs == FromLeft
                    then leftInverseBinaryTransformDec
                    else rightInverseBinaryTransformDec) $ averageBijection p
  in  detrans $ BLS.decData $ BLS.decodeBinList getValue encd
