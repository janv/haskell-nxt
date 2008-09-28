-- | Lowlevel communication
module NXT.Comm (
	-- * Comm types
	NXTPort(..),
	NXTBrick(..),
	NXTHandle,
	Message,
	-- * Open/Close
	nxtOpen,
	nxtClose,
	-- * Basic Read/Write
	nxtRead,
	nxtWrite,
	-- * Communication
	send,
	sendReceive,
	send0,
	send1,
	send2,
	send3,
	send4,
	send5,
	send6,
	send7
) where

import System.IO
import System.Posix
import Data.Bits
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import Data.Time.Clock
import NXT.Helpers
import NXT.Codes

------------------------------------------------------------------------------
-- Comm Types
------------------------------------------------------------------------------

data NXTPort   = Bluetooth | USB deriving Eq
-- | The string should contain the path to a character-device file
data NXTBrick  = NXTBrick NXTPort String
-- | IO Handle to a NXT Brick
data NXTHandle = NXTHandle NXTPort Handle (MVar UTCTime)
-- | Messages sent to/received from the brick
type Message   = B.ByteString

------------------------------------------------------------------------------
-- Open/Close
------------------------------------------------------------------------------

nxtOpen :: NXTBrick -> IO (NXTHandle)
nxtOpen (NXTBrick port dev) = do
	h <- openFile dev ReadWriteMode
	time <- newEmptyMVar
	hSetBuffering h NoBuffering
	return (NXTHandle port h time)

nxtClose :: NXTHandle -> IO ()
nxtClose (NXTHandle _ h _) = do
	hWaitForInput h 500
	hClose h

------------------------------------------------------------------------------
-- Bluetooth Helpers
------------------------------------------------------------------------------

-- | Add the BlueTooth length header in front of a message
btPack :: Message -> Message
btPack msg = B.append (littleEndianInt (B.length msg)) msg
-- TODO: Checken ob korrekt, evtl. verwirft das frühe Konvertieren 

-- | Strip the BlueTooth length header from a message
btUnPack :: Message -> Message
btUnPack m = if B.length m >= 2 then B.drop 2 m else error ("btUnPack: Message too small " ++ (debugByteString m) )


------------------------------------------------------------------------------
-- Basic Read/Write
------------------------------------------------------------------------------

-- | Read data from the brick
--   Does not work with USB yet
nxtRead :: NXTHandle -> IO (Message)
nxtRead (NXTHandle Bluetooth handle _) = do
	hFlush handle
	havinput  <- hWaitForInput handle 1000 -- Wait for Input
	btheader  <- if havinput then B.hGetNonBlocking handle 2 else (error "No Input") -- Could hang here if only 1 byte is ready
	msglength <- return (fromIntegral (int16FromWords (B.unpack btheader)))
	if msglength == 0
		then return B.empty
		else B.hGet handle (msglength)
nxtRead (NXTHandle USB handle _) = error "USB Read not implemented"

-- | Write data to the brick
nxtWrite :: NXTHandle -> Message -> IO ()
nxtWrite (NXTHandle port h _) m = do
	B.hPut h (if port == Bluetooth then btPack m else m)
	hFlush h

------------------------------------------------------------------------------
-- Communication
------------------------------------------------------------------------------

-- | Send a command to the NXT
send :: NXTHandle		-- ^ IO Handle
	-> CommandMode		-- ^ The CommandMode
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> IO ()
send handle mode cmd = do
	sendReceive handle mode False cmd
	return ()

-- | Receive Data from the NXT
receive :: NXTHandle -> IO Message
receive handle = nxtRead handle

-- | Most Basic Communication function
sendReceive :: NXTHandle	-- ^ IO Handle
	-> CommandMode		-- ^ The CommandMode
	-> Bool			-- ^ Indicate wether a reply is expected or not
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> IO (Maybe Message)	-- ^ Just the reply or Nothing	
sendReceive handle mode reply cmd = do
	nxtWrite handle message
	-- putStrLn (debugByteString message)
	if reply
		then do answer <- receive handle
			return (Just answer)
		else return Nothing
	where message = (commandType mode reply) `B.cons` cmd

------------------------------------------------------------------------------
-- Send Helpers
------------------------------------------------------------------------------

send0 mode msg h = mapResult0 (send h mode) msg
send1 mode msg h = mapResult1 (send h mode) msg
-- | Helper function used to create IO Actions more easily from 
--   NXT Message composition functions
--   
--   For NXT Messages with 2 Arguments. Other versions for different numbers
--   of arguments exist. See NXT.Commands for usage examples.
send2 :: CommandMode		-- ^ The commandMode to use
	-> (a -> b -> Message)	-- ^ The NXT Message Function
	-> NXTHandle
	-> a			-- ^ First argument to the NXT Message Function
	-> b			-- ^ Second argument to the NXT Message Function
	-> IO ()
send2 mode msg h = mapResult2 (send h mode) msg 
send3 mode msg h = mapResult3 (send h mode) msg 
send4 mode msg h = mapResult4 (send h mode) msg 
send5 mode msg h = mapResult5 (send h mode) msg 
send6 mode msg h = mapResult6 (send h mode) msg 
send7 mode msg h = mapResult7 (send h mode) msg
