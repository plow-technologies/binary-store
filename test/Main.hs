
import Data.BinaryList (BinList,Exponent)
import qualified Data.BinaryList as BL
import Data.BinaryList.Serialize (Direction (..), fromDecoded)
import Format.BinaryStore

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Control.Applicative (pure,empty,(<$>))
import qualified Data.Foldable as F

instance Arbitrary a => Arbitrary (BinList a) where
  arbitrary = do
    l <- choose (0,12 :: Exponent)
    BL.replicateA l arbitrary

instance Arbitrary Direction where
  arbitrary = elements [FromLeft,FromRight]

instance Arbitrary a => Arbitrary (TValue a) where
  arbitrary = oneof [pure empty, pure <$> arbitrary]

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

instance Approx a => Approx (TValue a) where
  tv ~= tv' = fromTValue tv ~= fromTValue tv'

--

main :: IO ()
main = defaultMain $ testGroup "binary-store"
  [ testGroup "Double"
    [ QC.testProperty "read/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList Double)
    , QC.testProperty "read/decode/encode/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= decode . encode >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList Double)
      ]
  , testGroup "Maybe Double"
    [ QC.testProperty "read/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList (Maybe Double))
    , QC.testProperty "read/decode/encode/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= decode . encode >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList (Maybe Double))
      ]
  , testGroup "TValue Double"
    [ QC.testProperty "read/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList (TValue Double))
    , QC.testProperty "read/decode/encode/create"
         $ \xs dr c bz -> forAll (choose (1,255))
         $ \d          -> forAll (choose (0,d))
         $ \n          -> ( createBinaryStore dr n d c bz xs >>= decode . encode >>= fromDecoded . readBinaryStore )
                       ~= Right (xs :: BinList (TValue Double))
      ]
    ]
