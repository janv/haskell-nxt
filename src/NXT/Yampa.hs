-- | Interface functions to Yampas reactimate action
module NXT.Yampa (
	-- * Reactimate function generators
	mkInit,
	mkSense,
	mkActuate,
	nxtSF
) where

import Data.Word
import Data.Int
import NXT.Codes
import NXT.Comm
import NXT.Commands
import NXT.Sensor
import FRP.Yampa


------------------------------------------------------------------------------
-- Basic Types
------------------------------------------------------------------------------

-- | Input from the brick, four inputValues, one for each port
data NXTInput = NXTInput {
	switchInput   :: Bool,
	soundInput    :: Float,
	lightInput    :: Float,
	distanceInput :: Word8,
	motorAState   :: OutputState,
	motorBState   :: OutputState,
	motorCState   :: OutputState
}

-- | Output to the Brick, in the Form of a list of messages to be sent
data NXTOutput = NXTOutput {
	driveMode   :: NXTDriveMode,
	motorCSpeed :: Int8
}

data NXTDriveMode =
      DriveStop
    | DriveDiff {
	  powerLeft  :: Int8,
	  powerRight :: Int8
      }
    | DriveTR {
	  power     :: Int8,
	  turnRatio :: Int8
      }

------------------------------------------------------------------------------
-- Reactimate functions
------------------------------------------------------------------------------

-- | Make a Yampa-compatible init-function by currying the NXTHandle parameter
mkInit :: NXTHandle -> IO NXTInput
mkInit h = do
	playtone h 500 500
	setupDefaultSensors h
	resetmotorposition h MotorA True
	resetmotorposition h MotorB True
	resetmotorposition h MotorC True
	playtone h 500 500
	i <- getNXTInput h
	nxtInitTime h
	return i

-- | Make a Yampa-compatible sense-function by currying the NXTHandle parameter
mkSense :: NXTHandle -> Bool -> IO (DTime, Maybe NXTInput)
mkSense h _ = do
	ni    <- getNXTInput h
	dtime <- nxtUpdateTime h
	return (dtime, Just ni)
	

-- | Make a Yampa-compatible actuate-function by currying the NXTHandle parameter
mkActuate:: NXTHandle -> Bool -> NXTOutput -> IO (Bool)
mkActuate h _ (NXTOutput driveMode motorCSpeed) = do
	case driveMode of
		DriveStop       -> stopmotors
		DriveDiff pl pr -> drivediff pl pr
		DriveTR pw tr   -> drivetr   pw tr
	setoutputstate h MotorC motorCSpeed [MotorOn, Regulated, Brake] MotorSpeed 0 motorCState 0
	return False
	where
	motorCState     = if motorCSpeed == 0 then RunStateIdle else Running
	stopmotors      = (setoutputstate h MotorA  0 [MotorOn, Regulated, Brake] MotorSpeed  0 RunStateIdle 0) >>
		          (setoutputstate h MotorB  0 [MotorOn, Regulated, Brake] MotorSpeed  0 RunStateIdle 0)
	drivediff pl pr = (setoutputstate h MotorA pl [MotorOn, Regulated, Brake] MotorSpeed  0 Running      0) >>
			  (setoutputstate h MotorB pr [MotorOn, Regulated, Brake] MotorSpeed  0 Running      0)
	drivetr   pw tr = (setoutputstate h MotorA pw [MotorOn, Regulated, Brake] MotorSync  tr Running      0) >>
			  (setoutputstate h MotorB pw [MotorOn, Regulated, Brake] MotorSync  tr Running      0)
------------------------------------------------------------------------------
-- Signal Transformer
------------------------------------------------------------------------------

nxtSF :: SF NXTInput NXTOutput
nxtSF = nxtButtonState >>> motorOut >>> nxtMotorOut


{- 
  Event generieren wenn Knopf gedrückt wird
  SF NXTInput 
  Signal über MotorState 1 in
  Beides kombinieren zu Event für Motor 1 an, Motor 1 aus
-}

-- | Based on a boolean input (e.g. from a switch), decide wether to turn on a
--   motor or not
motorOut :: SF Bool Bool
motorOut = arr id

nxtMotorOut :: SF Bool NXTOutput
nxtMotorOut = arr (\b -> if b then NXTOutput (DriveDiff 50 50) 0 else NXTOutput DriveStop 0 )

nxtButtonState :: SF NXTInput Bool
nxtButtonState = arr (\i -> switchInput i)

------------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------------

-- | Get NXT input from a NXTHandle
getNXTInput :: NXTHandle -> IO (NXTInput)
getNXTInput h = do
	inp1 <- getSwitch   h
	inp2 <- getSound    h
	inp3 <- getLight    h
	inp4 <- getDistance h
	motor1 <- getoutputstate h MotorA
	motor2 <- getoutputstate h MotorB
	motor3 <- getoutputstate h MotorC
	return (NXTInput inp1 inp2 inp3 inp4 motor1 motor2 motor3)
