module Lib where


import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Bits ((.|.), shift)
import Data.Char (chr)
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32)

toU8 :: Word8 -> Char
toU8 = chr . fromIntegral 

toS16 :: Word8 -> Word8 -> Int16
toS16 x y = fromIntegral z
  where x' = fromIntegral x :: Word16
        y' = fromIntegral y :: Word16
        z  = (shift x' 8) .|. y'

toS32 :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
toS32 a b c d = fromIntegral z
  where a' = fromIntegral a :: Word32
        b' = fromIntegral b :: Word32
        c' = fromIntegral c :: Word32
        d' = fromIntegral d :: Word32
        z = (shift a' 24) .|. (shift b' 16) .|. (shift c' 8) .|. d'

-- toF32 :: Word8 -> Word8 -> Word8 -> Word8 -> Float
-- toF32 a b c d = fromIntegral z
--   where x'

(==>) :: (L.ByteString -> Maybe (a, L.ByteString)) -> (a -> L.ByteString -> Maybe (b, L.ByteString)) -> (L.ByteString -> Maybe (b, L.ByteString))
(==>) f g bs = f bs >>= \(a, bs') -> g a bs'


parseS32 :: L.ByteString -> Maybe (Int32, L.ByteString)
parseS32 bs = undefined
        

readHeader :: L.ByteString -> Header
readHeader bs = undefined --Header(toS32)


data Header = Header {
  numberOfFrames :: Integer,
  numberOfTags :: Integer
}
