-- | High level motor commands
module NXT.Motor where

import NXT.Comm
import NXT.Commands
import NXT.Codes
import System.IO
import Data.List
import Data.Int
import Data.Word

-- | Change a motors brake setting
setBrake :: NXTHandle -> OutputPort -> Bool -> IO ()
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

-- | Change a motors regulation setting
setRegulation :: NXTHandle -> OutputPort -> RegulationMode -> IO ()
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
motorGo :: NXTHandle
	-> OutputPort
	-> Int8		-- ^ Speed [-100..100]
	-> IO ()
motorGo h port speed = do
	setoutputstate h port
		speed
		[MotorOn, Regulated]
		MotorSpeed
		0
		Running
		0

-- | Stop a Motor
motorStop :: NXTHandle -> OutputPort -> IO ()
motorStop h port = do
	setoutputstate h port
		0
		[Coast]
		RegulationIdle
		0
		RunStateIdle
		0

-- | Turn a motor a set amount
motorTurn :: NXTHandle -> OutputPort
	-> Int8		-- ^ Speed [-100..100]
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
