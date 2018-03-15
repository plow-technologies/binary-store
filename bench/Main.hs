
module Main (main) where

import Criterion.Main
import Data.BinaryList (BinList,Exponent)
import qualified Data.BinaryList as BL
import qualified Data.BinaryList.Serialize as BL
import qualified Format.BinaryStore as BS
import Control.Applicative
import Data.ByteString.Lazy (ByteString)

ch :: Int -> Exponent -> BinList (BS.TValue Double)
ch c e = fst $ BL.fromListSplit (pure 0) e $ cycle $ pure 2017 : replicate (c-1) empty

main :: IO ()
main = defaultMain
  [ bench "encode binary store" $
      nf (BS.encode . BS.createBinaryStoreDefault) (ch 15 18)
  , bench "decode binary store" $
      nf ((>>= BL.fromDecoded) . fmap BS.readBinaryStore . BS.decode :: ByteString -> Either String (BinList (BS.TValue Double)))
         (BS.encode $ BS.createBinaryStoreDefault $ ch 15 18)
    ]
