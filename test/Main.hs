
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL
import Data.BinaryList.Serialize (Direction (..), fromDecoded)
import Format.BinaryStore

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Data.Maybe (fromJust)
import qualified Data.Foldable as F

instance Arbitrary a => Arbitrary (BinList a) where
  arbitrary = do
    l <- choose (0,12 :: Int)
    xs <- vector (2^l)
    return $ fromJust $ BL.fromList xs

instance Arbitrary Direction where
  arbitrary = elements [FromLeft,FromRight]

instance Show Direction where
  show FromLeft = "FromLeft"
  show FromRight = "FromRight"

-- Approximately equal class

class Approx a where
  (~=) :: a -> a -> Bool

instance Approx Double where
  x ~= y = abs (x - y) <= 0.00001

instance Approx a => Approx (BinList a) where
  xs ~= ys = F.and $ BL.zipWith (~=) xs ys

instance Approx a => Approx (Either e a) where
  Right x ~= Right y = x ~= y
  Left _ ~= Left _ = True
  _ ~= _ = False

instance Approx a => Approx (Maybe a) where
  Just x ~= Just y = x ~= y
  Nothing ~= Nothing = True
  _ ~= _ = False

--

main :: IO ()
main = defaultMain $ testGroup "binary-store"
  [ testGroup "Double"
    [ QC.testProperty "read/create"
         $ \xs dr c -> forAll (choose (1,255))
         $ \d       -> forAll (choose (0,d))
         $ \n       -> ( createBinaryStore dr n d c xs >>= fromDecoded . readBinaryStore )
                    ~= Right (xs :: BinList Double)
    , QC.testProperty "read/decode/encode/create"
         $ \xs dr c -> forAll (choose (1,255))
         $ \d       -> forAll (choose (0,d))
         $ \n       -> ( createBinaryStore dr n d c xs >>= decode . encode >>= fromDecoded . readBinaryStore )
                    ~= Right (xs :: BinList Double)
      ]
  , testGroup "Maybe Double"
    [ QC.testProperty "read/create"
         $ \xs dr c -> forAll (choose (1,255))
         $ \d       -> forAll (choose (0,d))
         $ \n       -> ( createBinaryStore dr n d c xs >>= fromDecoded . readBinaryStore )
                    ~= Right (xs :: BinList (Maybe Double))
    , QC.testProperty "read/decode/encode/create"
         $ \xs dr c -> forAll (choose (1,255))
         $ \d       -> forAll (choose (0,d))
         $ \n       -> ( createBinaryStore dr n d c xs >>= decode . encode >>= fromDecoded . readBinaryStore )
                    ~= Right (xs :: BinList (Maybe Double))
      ]
    ]
