module NXT.Commands where

import NXT.Codes
import System.IO
import Data.Char
import Data.Word
import Data.Bits
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
-- instance Appendable Word8 where
-- 	toBS n = B.singleton n
-- instance Appendable [Char] where
-- 	toBS s = B.pack (fmap (fromIntegral . ord) s)
instance Integral a => Appendable [a] where
	toBS l = B.pack (fmap fromIntegral l)
-- instance Appendable Int where
-- 	toBS i = B.singleton (fromIntegral i)

------------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------------

-- | Convert a String into a ByteString
bsFromS :: String -> B.ByteString
bsFromS chars = B.pack (fmap (fromIntegral . ord) chars)

-- | Add the BlueTooth length header in front of a message
btPack :: B.ByteString -> B.ByteString
btPack msg = B.append (littleEndianPair (B.length msg)) msg
-- TODO: Checken ob korrekt, evtl. verwirft das frühe Konvertieren 

-- Convert an Integer to a little endian ByteString 
littleEndianPair :: Int -> B.ByteString
littleEndianPair i = B.pack [lsb, msb]
			where lsb = fromIntegral (i .&. 0xFF)
			      msb = fromIntegral (i `shiftR` 8)

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

mapResult2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
mapResult2 mapFun fun a b         = mapFun (fun a b)
mapResult3 mapFun fun a b c       = mapFun (fun a b c)
mapResult4 mapFun fun a b c d     = mapFun (fun a b c d)
mapResult5 mapFun fun a b c d e   = mapFun (fun a b c d e)
mapResult6 mapFun fun a b c d e f = mapFun (fun a b c d e f)

send2 :: CommandMode -> (a -> b -> Message) -> Handle -> a -> b -> IO()
send2 mode msg h = mapResult2 (send h mode) msg 
send3 mode msg h = mapResult3 (send h mode) msg 
send4 mode msg h = mapResult4 (send h mode) msg 
send5 mode msg h = mapResult5 (send h mode) msg 
send6 mode msg h = mapResult6 (send h mode) msg 


-- STARTPROGRAM
-- STOPPROGRAM
-- PLAYSOUNDFILE

playtoneMsg :: Int -> Int -> Message
-- playtoneMsg freq duration = 0x03 `B.cons` (B.append freqWord durWord)
playtoneMsg freq duration = [0x03] +++ freqWord  +++ durWord
				where freqWord = littleEndianPair freq
				      durWord  = littleEndianPair duration

playtone :: Handle -> Int -> Int -> IO ()
playtone = send2 Direct playtoneMsg