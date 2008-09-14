module NXT.Helpers where

import Data.Char
import Data.Word
import Data.Bits
import Data.Int
import qualified Data.ByteString as B

-- | Convert a String into a ByteString
bsFromS :: String -> B.ByteString
bsFromS chars = B.pack (fmap (fromIntegral . ord) chars)

-- Convert an Integer to a little endian ByteString 
-- The int is assumed not to exceed 16 bit
littleEndianInt :: Int -> B.ByteString
littleEndianInt i = B.pack [lsb, msb]
			where lsb = fromIntegral (i .&. 0xFF)
			      msb = fromIntegral (i `shiftR` 8)

-- Convert a Word16 value to a little-endian ByteString
littleEndianWord16 :: Word16 -> B.ByteString
littleEndianWord16 w = B.pack [lsb, msb]
			where lsb = fromIntegral ((w .&. 0x00FF) `shiftR` 0)
			      msb = fromIntegral ((w .&. 0xFF00) `shiftR` 8)

-- Convert a Word32 value to a little-endian ByteString
littleEndianWord32 :: Word32 -> B.ByteString
littleEndianWord32 w = B.pack [lsb, byte1, byte2, msb]
			where lsb   = fromIntegral ((w .&. 0x000000FF) `shiftR`  0)
			      byte1 = fromIntegral ((w .&. 0x0000FF00) `shiftR`  8)
			      byte2 = fromIntegral ((w .&. 0x00FF0000) `shiftR` 16)
			      msb   = fromIntegral ((w .&. 0xFF000000) `shiftR` 24)

word32FromWords :: [Word8] -> Word32
word32FromWords (a:b:c:d:[]) = (fromIntegral a) `shiftL` 24 + (fromIntegral b) `shiftL` 16 + (fromIntegral c) `shiftL` 8 + (fromIntegral d)
word16FromWords :: [Word8] -> Word16
word16FromWords (c:d:[]) = (fromIntegral c) `shiftL` 8 + (fromIntegral d)
int32FromWords :: [Word8] -> Int32
int32FromWords (a:b:c:d:[]) = fromIntegral (word32FromWords (a:b:c:d:[]))
int16FromWords :: [Word8] -> Int16
int16FromWords (c:d:[]) = fromIntegral (word16FromWords (c:d:[]))



debugByteString :: B.ByteString -> String
debugByteString b = B.foldr mapfun [] b
			where wordToHex w = charToHex (upperFour w) : charToHex (lowerFour w) : []
			      upperFour w = fromIntegral w `shiftR` 4
			      lowerFour w = fromIntegral w .&. 0xF
			      charToHex c = toUpper (intToDigit c)
			      mapfun w str = "0x" ++ (wordToHex w) ++ "(" ++ (show w) ++ ") " ++ str

-- | Helper that changes the result type of a function

-- TODO: Smells like an Arrow
mapResult2 :: (c -> d)		-- ^ Mapping between the result types
	-> (a -> b -> c)	-- ^ The original function
	-> (a -> b -> d)	-- ^ The resulting function

mapResult2 mapFun fun a b           = mapFun (fun a b)
mapResult3 mapFun fun a b c         = mapFun (fun a b c)
mapResult4 mapFun fun a b c d       = mapFun (fun a b c d)
mapResult5 mapFun fun a b c d e     = mapFun (fun a b c d e)
mapResult6 mapFun fun a b c d e f   = mapFun (fun a b c d e f)
mapResult7 mapFun fun a b c d e f g = mapFun (fun a b c d e f g)

segmentList :: [a] -> [Int] -> [[a]]
segmentList bs []     = []
segmentList bs (s:ss) = (take s bs) : (segmentList (drop s bs) ss)

pickSegment :: (Integral a, Num b) => [[a]] -> Int -> b
pickSegment segments n = fromIntegral (head (segments !! n))
