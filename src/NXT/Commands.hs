module NXT.Commands where

import NXT.Codes
import System.IO
import Data.Char
import Data.Word
import Data.Bits
import qualified Data.ByteString as B

type Message = B.ByteString

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
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> CommandMode		-- ^ The CommandMode
	-> IO ()
send handle cmd mode = do
	sendReceive handle cmd mode False
	return ()

-- | Receive Data from the NXT
receive :: Handle
	-> IO Message
receive handle = B.hGetContents handle

-- | Most Basic Communication function
sendReceive :: Handle		-- ^ IO Handle
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> CommandMode		-- ^ The CommandMode
	-> Bool			-- ^ Indicate wether a reply is expected or not
	-> IO (Maybe Message)	-- ^ Just The reply or Nothing	
sendReceive handle cmd mode reply = do
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

-- TODO Einheitliche Rückgabe, leere Listen?

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

-- STARTPROGRAM
-- STOPPROGRAM
-- PLAYSOUNDFILE

playtoneMsg :: Int -> Int -> Message
playtoneMsg freq duration = 0x03 `B.cons` (B.append freqWord durWord)
				where freqWord = littleEndianPair freq
				      durWord  = littleEndianPair duration

playtone :: Handle -> Int -> Int -> IO ()
playtone handle freq duration = send handle (playtoneMsg freq duration) Direct