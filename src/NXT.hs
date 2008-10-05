module NXT where

import NXT.Comm
import NXT.Commands
import NXT.Codes
import NXT.Helpers
import NXT.Motor
import NXT.Sensor
import qualified Data.ByteString as B
import System.Posix
import Data.Time.Clock

btBrick = NXTBrick Bluetooth "/dev/tty.NXT-DevB-1"

testloop = do
	h <- nxtOpen btBrick
	playtone h 500 500
	setupDefaultSensors h
	xxx <- getCurrentTime
	let rec = do 
		s <- getSwitch h
		putStrLn (show s)
		now <- getCurrentTime
		putStrLn (show (diffUTCTime now xxx) )
		usleep (1000 * 30)
		-- if s then motorGo h MotorA 50 else motorStop h MotorA
		rec
	rec
	nxtClose h