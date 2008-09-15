module NXT.Commands where

import NXT.Codes
import NXT.Helpers
import System.IO
import Data.Char
import Data.Word
import Data.Bits
import Data.Int
import qualified Data.ByteString as B

type Message = B.ByteString

------------------------------------------------------------------------------
-- Appendable Class
------------------------------------------------------------------------------

-- | Support easy construction of NXT Command Messages
class Appendable a where
	toBS :: a -> B.ByteString

(+++) :: (Appendable a, Appendable b) => a -> b -> B.ByteString
x +++ y = B.append (toBS x) (toBS y)
	
instance Appendable B.ByteString where
	toBS bs = bs
instance Appendable Word8 where
	toBS x = B.singleton x
instance Appendable [Char] where
	toBS s = B.pack (fmap (fromIntegral . ord) s)
instance Appendable Int  where toBS i = B.singleton (fromIntegral i)
instance Appendable Int8 where toBS i = B.singleton (fromIntegral i)
instance Appendable OutputPort     where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable OutputMode     where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable RegulationMode where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable RunState       where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable InputPort      where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable SensorType     where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable SensorMode     where toBS p = B.singleton (fromIntegral (fromEnum p))
instance Appendable Bool           where toBS p = B.singleton (if p then (0x01::Word8) else (0x00::Word8))
-- instance (Enum a) => Appendable a where
-- 	toBS x = B.singleton (fromEnum x)
-- instance Integral a => Appendable [a] where
-- 	toBS l = B.pack (fmap fromIntegral l)

------------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------------

-- | Add the BlueTooth length header in front of a message
btPack :: B.ByteString -> B.ByteString
btPack msg = B.append (littleEndianInt (B.length msg)) msg
-- TODO: Checken ob korrekt, evtl. verwirft das frühe Konvertieren 

-- | Strip the BlueTooth length header from a message
btUnPack :: Message -> Message
btUnPack m = if assumedLength == (indicatedLength ws)
		then B.drop 2 m
		else error "Message length mismatch"
	where ws = B.unpack m 
	      assumedLength   = (B.length m) - 2
	      indicatedLength (lsb:msb:msg) = ((fromIntegral msb) `shiftL` 8) + (fromIntegral lsb)

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
--
--   Works only with the bluetooth interface currently
--   (length header read and strippend from messages)
receive :: Handle -> IO Message
receive handle = do
	hFlush handle
	btheader  <- B.hGet handle 2
	if 0 == B.length btheader
		then return B.empty
		else do msglength <- return (fromIntegral (int16FromWords (B.unpack btheader)))
			if msglength == 0
				then return B.empty
				else B.hGet handle (msglength)

-- | Most Basic Communication function
--
--   Works only wth the bluetooth interface currently
--   (length header is added to/stripped from messages)
sendReceive :: Handle		-- ^ IO Handle
	-> CommandMode		-- ^ The CommandMode
	-> Bool			-- ^ Indicate wether a reply is expected or not
	-> Message		-- ^ The Command to send, not including the Commandmode
	-> IO (Maybe Message)	-- ^ Just The reply or Nothing	
sendReceive handle mode reply cmd = do
	B.hPut handle message
	-- putStrLn (debugByteString message)
	hFlush handle
	if reply
		then do answer <- receive handle
			return (Just answer)
		else return Nothing
	where message     = btPack cmdWithMode
	      cmdWithMode = (commandType mode reply) `B.cons` cmd

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
send1 mode msg h = mapResult1 (send h mode) msg
send2 mode msg h = mapResult2 (send h mode) msg 
send3 mode msg h = mapResult3 (send h mode) msg 
send4 mode msg h = mapResult4 (send h mode) msg 
send5 mode msg h = mapResult5 (send h mode) msg 
send6 mode msg h = mapResult6 (send h mode) msg 
send7 mode msg h = mapResult7 (send h mode) msg

------------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------------

-- STARTPROGRAM
-- STOPPROGRAM
-- PLAYSOUNDFILE

-- PLAYTONE
playtoneMsg :: Word16 -> Word16 -> Message
playtoneMsg freq duration = "\x03" +++ freqWord  +++ durWord
				where freqWord = littleEndianWord16 freq
				      durWord  = littleEndianWord16 duration
playtone :: Handle -> Word16 -> Word16 -> IO ()
playtone = send2 Direct playtoneMsg

-- SETOUTPUTSTATE
setoutputstateMsg :: OutputPort
	-> Int8			-- ^ Power -100 - 100
	-> [OutputMode]
	-> RegulationMode
	-> Int8			-- ^ Turn Ratio SBYTE (-100 - 100)
	-> RunState		-- UBYTE
	-> Word32		-- ^ Tacho Limit (ULONG 0:Run Forever)
	-> Message
setoutputstateMsg po pw oms rm tr rs tl = "\x04" +++ po +++ pw +++ om +++ rm +++ tr +++ rs +++ (littleEndianWord32 tl)
	where om = bitfieldFromEnum oms
setoutputstate = send7 Direct setoutputstateMsg

-- SETINPUTMODE
setinputmodeMsg :: InputPort -> SensorType -> SensorMode -> Message
setinputmodeMsg port sensortype mode = "\x05" +++ port +++ sensortype +++ mode
setinputmode = send3 Direct setinputmodeMsg

-- GETOUTPUTSTATE
getoutputstateMsg :: OutputPort -> Message
getoutputstateMsg port = "\x06" +++ port -- TODO Range 0-2

getoutputstate :: Handle -> OutputPort -> IO (OutputState)
getoutputstate h port = do
	reply <- sendReceive h Direct True (getoutputstateMsg port)
	case reply of
		Nothing -> error "Getting Outputstate: No Reply"
		Just m  -> return (os m)
	
data OutputState =
	OutputState {
		outputPort      :: OutputPort,
		powerSetPoint   :: Int8,
		outputMode      :: [OutputMode],
		regulationMode  :: RegulationMode,
		turnRatio       :: Int8,
		runState        :: RunState,
		tachoLimit      :: Word32,	-- ^ Current limit on a movement in progress if any
		tachoCount      :: Int32,	-- ^ Internal count. Number of counts since last reset of the motor
		blockTachoCount :: Int32,	-- ^ Current position relative to the last programmed movement
		rotationCount   :: Int32	-- ^ Current position relative to last reset of the rotation sensor for this motor
	} deriving Show
os :: Message -> OutputState
os m = OutputState port power oms rm tr rs tl tc btc rc
	where parts = segmentList (B.unpack m) [3, 1, 1, 1, 1, 1, 1, 4, 4, 4, 4]
	      port  = toEnum           (pickSegment parts 1)
	      power = fromIntegral     (pickSegment parts 2)
	      oms   = enumFromBitfield (pickSegment parts 3)
	      rm    = toEnum           (pickSegment parts 4)
	      tr    = fromIntegral     (pickSegment parts 5)
	      rs    = toEnum           (pickSegment parts 6)
	      tl    = word32FromWords  (parts !!  7)
	      tc    = int32FromWords   (parts !!  8)
	      btc   = int32FromWords   (parts !!  9)
	      rc    = int32FromWords   (parts !! 10)

-- GETINPUTVALUES
getinputvaluesMsg :: InputPort -> Message
getinputvaluesMsg port = "\x07" +++ port

getinputvalues :: Handle -> InputPort -> IO (InputValues)
getinputvalues h port = do
	reply <- sendReceive h Direct True (getinputvaluesMsg port)
	case reply of
		Nothing -> error "Getting Inputvalues: No Reply"
		Just m  -> return (iv m)

data InputValues =
	InputValues {
		port             :: InputPort,
		valid            :: Bool,
		calibrated       :: Bool,
		sensorType       :: SensorType,
		sensorMode       :: SensorMode,
		raw              :: Word16,
		normalized       :: Word16,
		scaled           :: Int16,
		calibrated_value :: Int16
	} deriving Show
iv :: Message -> InputValues
iv m = InputValues p v c st sm r n s cv
	where parts = segmentList (B.unpack m) [3, 1, 1, 1, 1, 1, 2, 2, 2, 2]
	      p  = toEnum (pickSegment parts 1)
	      v  =   1 == (pickSegment parts 2)
	      c  =   1 == (pickSegment parts 3)
	      st = toEnum (pickSegment parts 4)
	      sm = toEnum (pickSegment parts 5)
	      r  = word16FromWords (parts !! 6)
	      n  = word16FromWords (parts !! 7)
	      s  = int16FromWords  (parts !! 8)
	      cv = int16FromWords  (parts !! 9)

-- RESETINPUTSCALEDVALUE
resetinputscaledvalueMsg :: InputPort -> Message
resetinputscaledvalueMsg port = "\x08" +++ port
resetinputscaledvalue = send1 Direct resetinputscaledvalueMsg

-- MESSAGEWRITE

-- RESETMOTORPOSITION
resetmotorpositionMsg :: OutputPort
	-> Bool	-- ^ Relative? (True: relative to last movement, False: absolute position)
	-> Message
resetmotorpositionMsg port relative = "\x0A" +++ port +++ relative
resetmotorposition = send2 Direct resetmotorpositionMsg

-- GETBATTERYLEVEL
getbatterylevelMsg :: Message
getbatterylevelMsg = B.singleton 0x0B
getbatterylevel :: Handle -> IO (Word16)
getbatterylevel h = do
	reply <- sendReceive h Direct True getbatterylevelMsg
	case reply of
		Nothing -> error "getbatterylevel: No Reply"
		Just m  -> do let parts = segmentList (B.unpack m) [3, 2]
			          level = word16FromWords (parts !! 1)
			      return level

-- STOPSOUNDPLAYBACK
stopsoundplaybackMsg :: Message
stopsoundplaybackMsg = B.singleton 0x0C
stopsoundplayback handle = send handle Direct stopsoundplaybackMsg

-- KEEPALIVE
keepaliveMsg :: Message
keepaliveMsg = B.singleton 0x0D
keepalive handle = send handle Direct keepaliveMsg
-- TODO: Receive sleep time limit

-- LSGETSTATUS
-- LSWRITE
-- LSREAD
-- GETCURRENTROGRAMME
-- MESSAGEREAD