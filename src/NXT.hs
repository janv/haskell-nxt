module NXT where
	
	import NXT.Commands
	import NXT.Codes
	import NXT.Helpers
	import System.IO
	import System.Posix
	import qualified Data.ByteString as B
	
	testloop = do
		h <- openFile "/dev/tty.NXT-DevB-1" ReadWriteMode
		hSetBuffering h NoBuffering
		-- B.hPut h (B.pack [6,0,128,3,244,1,244,1])
		playtone h 500 500
		hFlush h
		setinputmode h Sensor1 Switch BooleanMode
		r <- getinputvalues h Sensor1
		putStrLn (show r)
{-		-- putStrLn  (debugByteString (setoutputstateMsg MotorA 50 MotorOn MotorSpeed 50 Running 20))
		setoutputstate h MotorA 30 [MotorOn, Brake, Regulated] MotorSpeed 0 Running 0
		hFlush h
		sleep 1
		setoutputstate h MotorA (-30) [MotorOn, Brake, Regulated] MotorSpeed 0 Running 0
		hFlush h
		sleep 1
		os <- getoutputstate h MotorA
		putStrLn (show os)
		hFlush h
		setoutputstate h MotorA 0 [MotorOn, Brake, Regulated] MotorSpeed 0 RunStateIdle 0
		hFlush h
		sleep 1
		setoutputstate h MotorA 0 [Coast] RegulationIdle 0 RunStateIdle 0
		hFlush h
-}
		hClose h