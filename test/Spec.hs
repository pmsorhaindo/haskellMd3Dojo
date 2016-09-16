{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.ByteString.Internal (c2w)
import Lib

prop_toU8 :: Char -> Bool
prop_toU8 c = c' == c
  where w = c2w c
        c' = toU8 w

runTests :: IO ()
runTests = quickCheck prop_toU8

main :: IO ()
main = runTests

