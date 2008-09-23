module NXT where

import NXT.Comm
import NXT.Commands
import NXT.Codes
import NXT.Helpers
import NXT.Motor
import NXT.Sensor
import qualified Data.ByteString as B

btBrick = NXTBrick Bluetooth "/dev/tty.NXT-DevB-1"

testloop = do
	h <- nxtOpen btBrick
	-- B.hPut h (B.pack [6,0,128,3,244,1,244,1])
	playtone h 500 500
	setupDefaultSensors h
	s <- getDistance h
	putStrLn (show s)
	-- a <- B.hGet h 5
	-- putStrLn (show (debugByteString a))
	
{-		-- putStrLn  (debugByteString (setoutputstateMsg MotorA 50 MotorOn MotorSpeed 50 Running 20))
	setoutputstate h MotorA 30 [MotorOn, Brake, Regulated] MotorSpeed 0 Running 0
	sleep 1
	setoutputstate h MotorA (-30) [MotorOn, Brake, Regulated] MotorSpeed 0 Running 0
	sleep 1
	os <- getoutputstate h MotorA
	putStrLn (show os)
	setoutputstate h MotorA 0 [MotorOn, Brake, Regulated] MotorSpeed 0 RunStateIdle 0
	sleep 1
	setoutputstate h MotorA 0 [Coast] RegulationIdle 0 RunStateIdle 0
-}
	-- hWaitForInput h 250
	nxtClose h