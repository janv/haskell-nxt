{-# OPTIONS_GHC -XFlexibleInstances #-}
module NXT.Commands (
	-- * Appendable class
	Appendable,
	(+++),
	-- * Commands
	playtone,
	setoutputstate,
	setoutputstateMsg, -- TODO remove again, only here for testing purposes in Yampa.hs
	setinputmode,
	getoutputstate,
	getinputvalues,
	resetmotorposition,
	getbatterylevel,
	keepalive,
	lsgetstatus,
	lswrite,
	lsread,
	-- * Types
	-- ** Output
	OutputState,
		outputPort,
		powerSetPoint,
		outputMode,     
		regulationMode, 
		turnRatio,      
		runState,       
		tachoLimit,     
		tachoCount,     
		blockTachoCount,
		rotationCount,  
	-- ** Input
	InputValues,
		port            ,
		valid           ,
		calibrated      ,
		sensorType      ,
		sensorMode      ,
		raw             ,
		normalized      ,
		scaled          ,
		calibrated_value

) where

import NXT.Comm
import NXT.Codes
import NXT.Helpers
import System.IO
import Data.Char
import Data.Word
import Data.Bits
import Data.Int
import qualified Data.ByteString as B

------------------------------------------------------------------------------
-- Appendable Class
------------------------------------------------------------------------------

-- | Support easy construction of NXT Command Messages
class Appendable a where
	toBS :: a -> B.ByteString

-- | Combines two elements to a bytestring
(+++) :: (Appendable a, Appendable b) => a -> b -> B.ByteString
x +++ y = B.append (toBS x) (toBS y)
	
instance Appendable B.ByteString where
	toBS = id
instance Appendable Word8 where
	toBS = B.singleton
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
playtone :: NXTHandle
	-> Word16	-- ^ Frequency (200 - 14.000 Hz)
	-> Word16	-- ^ Duration
	-> IO ()
playtone = send2 Direct playtoneMsg

-- SETOUTPUTSTATE
setoutputstate :: NXTHandle
	-> OutputPort 		-- TODO: Version die OutputState als Parameter nimmt
	-> Int8			-- ^ Power -100 - 100
	-> [OutputMode]		-- ^ Combine some of the Outputmodes here
	-> RegulationMode	-- ^ For this to be effective, 'Regulated' has to be in the OutputMode list
	-> Int8			-- ^ Turn Ratio SBYTE (-100 - 100)
	-> RunState
	-> Word32		-- ^ Tacho Limit (ULONG 0:Run Forever)
	-> IO ()
setoutputstate = send7 Direct setoutputstateMsg
setoutputstateMsg :: OutputPort -> Int8 -> [OutputMode] -> RegulationMode -> Int8 -> RunState -> Word32 -> Message
setoutputstateMsg po pw oms rm tr rs tl = "\x04" +++ po +++ pw +++ om +++ rm +++ tr +++ rs +++ (littleEndianWord32 tl)
	where om = bitfieldFromEnum oms

-- SETINPUTMODE
-- | Set the input mode for one of the sensor ports
setinputmode :: NXTHandle -> InputPort -> SensorType -> SensorMode -> IO ()
setinputmode = send3 Direct setinputmodeMsg
setinputmodeMsg port sensortype mode = "\x05" +++ port +++ sensortype +++ mode

-- GETOUTPUTSTATE
getoutputstateMsg port = "\x06" +++ port -- TODO Range 0-2
-- | Retrieve the outputstate for a given motor port
getoutputstate :: NXTHandle -> OutputPort -> IO (OutputState)
getoutputstate h port = do
	reply <- sendReceive h Direct True (getoutputstateMsg port)
	case reply of
		Nothing -> error "Getting Outputstate: No Reply"
		Just m  -> return (os m)
-- | Please refer to the documentation of 'setoutputstate' for an explanation of the members
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

getinputvalues :: NXTHandle -> InputPort -> IO (InputValues)
getinputvalues h port = do
	reply <- sendReceive h Direct True (getinputvaluesMsg port)
	case reply of
		Nothing -> error "Getting Inputvalues: No Reply"
		Just m  -> return (iv m)

data InputValues =
	InputValues {
		port             :: InputPort,
		valid            :: Bool,	-- ^ Indicates wether the data read might be invalid due to
						--   unprocessed sensor changes.
		calibrated       :: Bool,
		sensorType       :: SensorType,
		sensorMode       :: SensorMode,
		raw              :: Word16,	-- ^ The Raw values as sent by the sensor
		normalized       :: Word16,	-- ^ The sensors values normalized over the range 0..1023
		scaled           :: Int16,	-- ^ Sensor values scaled according to the sensor.
						--   See 'NXT.Codes.SensorMode' for a description of the ranges
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
-- | Reset the scaled value on a input port.
resetinputscaledvalue :: NXTHandle -> InputPort -> IO ()
resetinputscaledvalue = send1 Direct resetinputscaledvalueMsg
resetinputscaledvalueMsg port = "\x08" +++ port

-- MESSAGEWRITE

-- RESETMOTORPOSITION
-- | Reset the position of an output motor port.
resetmotorposition :: NXTHandle
	-> OutputPort
	-> Bool			-- ^ Relative? (True: relative to last movement, False: absolute position)
	-> IO ()
resetmotorposition = send2 Direct resetmotorpositionMsg
resetmotorpositionMsg port relative = "\x0A" +++ port +++ relative

-- GETBATTERYLEVEL
getbatterylevelMsg :: Message
getbatterylevelMsg = B.singleton 0x0B
-- | Read the bricks battery level (voltage in millivolts)
getbatterylevel :: NXTHandle -> IO (Word16)
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
stopsoundplayback = send0 Direct stopsoundplaybackMsg

-- KEEPALIVE
keepaliveMsg :: Message
keepaliveMsg = B.singleton 0x0D
-- | Prevent the NXT from going to sleep
keepalive :: NXTHandle -> IO ()
keepalive = send0 Direct keepaliveMsg
-- TODO: Receive sleep time limit

-- LSGETSTATUS
lsgetstatusMsg :: InputPort -> Message
lsgetstatusMsg port = "\x0E" +++ port
-- | Count the available bytes to read on a lowspeed (I2C) port
lsgetstatus :: NXTHandle -> InputPort -> IO (Word8)
lsgetstatus h port = do
	reply <- sendReceive h Direct True (lsgetstatusMsg port)
	case reply of
		Nothing -> error "lsgetstatus: No Reply"
		Just m  -> do let parts = segmentList (B.unpack m) [3,1]
			      return (pickSegment parts 1)

-- LSWRITE
lswriteMsg :: InputPort
	-> Message	-- ^ The message to send over I2C
	-> Word8	-- ^ Rx data length
	-> Message
lswriteMsg port m rxl = "\x0F" +++ port +++ l +++ rxl +++ m
	where l = if B.length m > 16
		then error ("lswrite: Message too long " ++ (debugByteString m))
		else B.length m
-- | Write a command to a lowspeed communication port (I2C)
lswrite :: NXTHandle
	-> InputPort
	-> Message	-- ^ The messge to send over I2C
	-> Word8	-- ^ Rx datalength (expected return length)
	-> IO ()
lswrite = send3 Direct lswriteMsg

-- LSREAD
lsreadMsg :: InputPort -> Message
lsreadMsg port = "\x10" +++ port
-- | Read a reply from a lowspeed communication port (I2C)
lsread :: NXTHandle -> InputPort -> IO (Message)
lsread h port = do
	reply <- sendReceive h Direct True (lsreadMsg port)
	case reply of
		Nothing -> error "lsread: No Reply"
		Just m  -> do let parts  = segmentList (B.unpack m) [3, 1, 16]
			          len    = pickSegment parts 1
			          answer = take len (parts !! 2)
			      return (B.pack answer)

-- GETCURRENTROGRAMME
-- MESSAGEREAD