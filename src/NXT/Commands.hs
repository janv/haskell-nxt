module NXT.Commands where

import NXT.Codes
import System.IO
import Data.Char
import Data.Word
import Data.Bits
import Data.Int
import qualified Data.ByteString as B

type Message = B.ByteString


-- class Byte b where
-- 	toSingleton :: b -> Message
-- 	(+)
-- 	-- Instanzen: Word8, Int, Char
-- 	-- Methoden zur Verkettung
-- 	-- Einfache Verkettung von Einzelwerten zu Arrays
-- 	-- show instanz so dass die Hex-Werte angezeigt werden
-- 	-- ByteString erzeugen aus Word8 Array

-- instance B.ByteString [Byte]

(+++) :: (Appendable a, Appendable b) => a -> b -> B.ByteString
x +++ y = B.append (toBS x) (toBS y)

class Appendable a where
	toBS :: a -> B.ByteString

instance Appendable B.ByteString where
	toBS bs = bs
instance Appendable Word8 where
	toBS x = B.singleton x
-- instance (Enum a) => Appendable a where
-- 	toBS x = B.singleton (fromEnum x)
instance Appendable [Char] where
	toBS s = B.pack (fmap (fromIntegral . ord) s)
-- instance Integral a => Appendable [a] where
-- 	toBS l = B.pack (fmap fromIntegral l)
instance Appendable Int where
	toBS i = B.singleton (fromIntegral i)
instance Appendable Int8 where
	toBS i = B.singleton (fromIntegral i)
instance Appendable OutputPort where
	toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable OutputMode where
	toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable RegulationMode where
	toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable RunState where
	toBS p = B.singleton (fromIntegral (fromEnum p))

------------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------------

-- | Convert a String into a ByteString
bsFromS :: String -> B.ByteString
bsFromS chars = B.pack (fmap (fromIntegral . ord) chars)

-- | Add the BlueTooth length header in front of a message
btPack :: B.ByteString -> B.ByteString
btPack msg = B.append (littleEndianInt (B.length msg)) msg
-- TODO: Checken ob korrekt, evtl. verwirft das frühe Konvertieren 

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

debugByteString :: B.ByteString -> String
debugByteString b = B.foldr mapfun [] b
			where wordToHex w = charToHex (upperFour w) : charToHex (lowerFour w) : []
			      upperFour w = fromIntegral w `shiftR` 4
			      lowerFour w = fromIntegral w .&. 0xF
			      charToHex c = toUpper (intToDigit c)
			      mapfun w str = "0x" ++ (wordToHex w) ++ "(" ++ (show w) ++ ") " ++ str

------------------------------------------------------------------------------
-- Send and receive
------------------------------------------------------------------------------

-- | Send a command to the NXT
send :: Handle			-- ^ IO Handle
	-> CommandMode		-- ^ The CommandMode
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> IO ()
send handle mode cmd = do
	sendReceive handle mode False cmd
	return ()

-- | Receive Data from the NXT
receive :: Handle
	-> IO Message
receive handle = B.hGetContents handle

-- | Most Basic Communication function
sendReceive :: Handle		-- ^ IO Handle
	-> CommandMode		-- ^ The CommandMode
	-> Bool			-- ^ Indicate wether a reply is expected or not
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> IO (Maybe Message)	-- ^ Just The reply or Nothing	
sendReceive handle mode reply cmd = do
	B.hPut handle message
	-- putStrLn (show (B.unpack cmd))
	-- putStrLn (show (B.unpack cmdWithMode))
	-- putStrLn (show (B.unpack message))
	if reply
		then do answer <- receive handle
			return (Just answer)
		else return Nothing
	where message     = btPack cmdWithMode
	      cmdWithMode = (commandType mode reply) `B.cons` cmd

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

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


-- | Helper function used to create IO Actions more easily from 
--   NXT Message composition functions
--   
--   For NXT Messages with 2 Arguments
send2 :: CommandMode		-- ^ The commandMode to use
	-> (a -> b -> Message)	-- ^ The NXT Message Function
	-> Handle		-- ^ An IO Handle
	-> a			-- ^ First argument to the NXT Message Function
	-> b			-- ^ Second argument to the NXT Message Function
	-> IO()
	
send2 mode msg h = mapResult2 (send h mode) msg 
send3 mode msg h = mapResult3 (send h mode) msg 
send4 mode msg h = mapResult4 (send h mode) msg 
send5 mode msg h = mapResult5 (send h mode) msg 
send6 mode msg h = mapResult6 (send h mode) msg 
send7 mode msg h = mapResult7 (send h mode) msg 


-- STARTPROGRAM
-- STOPPROGRAM
-- PLAYSOUNDFILE

playtoneMsg :: Word16 -> Word16 -> Message
playtoneMsg freq duration = "\x03" +++ freqWord  +++ durWord
				where freqWord = littleEndianWord16 freq
				      durWord  = littleEndianWord16 duration

playtone :: Handle -> Word16 -> Word16 -> IO ()
playtone = send2 Direct playtoneMsg

setoutputstateMsg :: OutputPort
	-> Int8			-- ^ Power -100 - 100
	-> OutputMode
	-> RegulationMode
	-> Int8			-- ^ Turn Ratio SBYTE (-100 - 100)
	-> RunState		-- UBYTE
	-> Word32		-- ^ Tacho Limit (ULONG 0:Run Forever)
	-> Message
setoutputstateMsg po pw om rm tr rs tl = "\x04" +++ po +++ pw +++ om +++ rm +++ tr +++ rs +++ (littleEndianWord32 tl)

setoutputstate = send7 Direct setoutputstateMsg