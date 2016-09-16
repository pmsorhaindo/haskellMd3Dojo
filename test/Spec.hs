import Test.QuickCheck
import Test.QuickCheck.Batch
import Test.QuickCheck.Arbitrary
import Data.ByteString.Internal (c2w)
import Lib

options = TestOptions {

}

main :: IO ()
main = do
  runTests "simple" options [run prop_toU8]


prop_toU8 :: Char -> Bool
prop_toU8 c = c' == c
  where w = c2w c
        c' = toU8 w

