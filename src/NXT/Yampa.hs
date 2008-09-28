-- | Interface functions to Yampas reactimate action
module NXT.Yampa (
	-- * Basic Types
	NXTInput,
	NXTOutput,
	-- * Reactimate function generators
	mkInit,
	mkSense,
	mkActuate
) where

import Data.Word
import NXT.Codes
import NXT.Comm
import NXT.Commands
import NXT.Sensor
import FRP.Yampa


------------------------------------------------------------------------------
-- Basic Types
------------------------------------------------------------------------------

-- | Input from the brick, four inputValues, one for each port
type NXTInput = ((Bool, Float, Float, Word8), (OutputState, OutputState, OutputState))

-- | Output to the Brick, in the Form of a list of messages to be sent
type NXTOutput = [Message]

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
mkActuate h _ nxto = do
	if nxto == []
		then return True
		else do
			let sendMessages []     = return ()
			    sendMessages (m:ms) = do
				send h Direct m
				sendMessages ms
				return ()
			sendMessages nxto
			return False

------------------------------------------------------------------------------
-- Signal Transformer
------------------------------------------------------------------------------

-- nxtSF :: SF NXTInput NXTOutput
-- nxtSF = arr nxtSFFun
-- 
-- nxtSFFun :: NXTInput -> NXTOutput

{- 
  Event generieren wenn Knopf gedrückt wird
  SF NXTInput 
  Signal über MotorState 1 in
  Beides kombinieren zu Event für Motor 1 an, Motor 1 aus
-}

motorOut :: SF Bool Bool
motorOut = arr id

------------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------------

getNXTInput :: NXTHandle -> IO (NXTInput)
getNXTInput h = do
	inp1 <- getSwitch   h
	inp2 <- getSound    h
	inp3 <- getLight    h
	inp4 <- getDistance h
	motor1 <- getoutputstate h MotorA
	motor2 <- getoutputstate h MotorB
	motor3 <- getoutputstate h MotorC
	return ((inp1, inp2, inp3, inp4), (motor1, motor2, motor3))
