module NXT where

import NXT.Commands
import NXT.Codes
import NXT.Helpers
import NXT.Motor
import NXT.Sensor
import System.IO
import System.Posix
import qualified Data.ByteString as B

testloop = do
	h <- openFile "/dev/tty.NXT-DevB-1" ReadWriteMode
	hSetBuffering h NoBuffering
	-- B.hPut h (B.pack [6,0,128,3,244,1,244,1])
	playtone h 500 500
	-- sleep 2
	-- setinputmode h Sensor4 Switch BooleanMode
	setupDefaultSensors h
	s <- getLight h
	putStrLn (show s)
	
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
	sleep 1
	hFlush h
	hClose h