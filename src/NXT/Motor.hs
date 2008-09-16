module NXT.Motor where

import NXT.Commands
import NXT.Codes
import System.IO
import Data.List
import Data.Int
import Data.Word

-- | Change a motors brake setting
setBrake :: Handle -> OutputPort -> Bool -> IO ()
setBrake h port onoff = do
	os <- getoutputstate h port
	setoutputstate h port
		(powerSetPoint os)
		(modify (outputMode os))
		(regulationMode os)
		(turnRatio os)
		(runState os)
		(tachoLimit os)  -- evtl. tacholimit - blocktachocount
	where modify l = if onoff then delete Brake l else l

setRegulation :: Handle -> OutputPort -> RegulationMode -> IO ()
setRegulation h port regMode = do
	os <- getoutputstate h port
	setoutputstate h port
		(powerSetPoint os)
		(modMode (outputMode os))
		(regMode)
		(turnRatio os)
		(runState os)
		(tachoLimit os)  -- evtl. tacholimit - blocktachocount
	where modMode l = case regMode of
		MotorSpeed     -> Regulated:l
		MotorSync      -> Regulated:l
		RegulationIdle -> delete Regulated l

-- | Run a motor
motorGo :: Handle -> OutputPort -> Int8 -> IO ()
motorGo h port speed = do
	setoutputstate h port
		speed
		[MotorOn, Regulated]
		MotorSpeed
		0
		Running
		0

-- | Stop a Motor
motorStop :: Handle -> OutputPort -> IO ()
motorStop h port = do
	setoutputstate h port
		0
		[Coast]
		RegulationIdle
		0
		RunStateIdle
		0

-- | Turn a motor a set amount
motorTurn :: Handle -> OutputPort
	-> Int8		-- ^ Speed
	-> Word32	-- ^ Degrees of rotation
	-> IO ()
motorTurn h port speed dist = do
	setoutputstate h port
		speed
		[MotorOn, Regulated, Brake]
		MotorSpeed
		0
		Running
		dist
