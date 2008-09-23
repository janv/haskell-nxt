module NXT.Sensor where

import NXT.Comm
import NXT.Commands
import NXT.Codes
import NXT.Ultrasonic
import System.IO
import System.Posix
import Data.List
import Data.Int
import Data.Word
import qualified Data.ByteString as B

setupDefaultSensors :: NXTHandle -> IO ()
setupDefaultSensors h = do
	setinputmode h Sensor1 Switch        BooleanMode
	setinputmode h Sensor2 SoundDb       RawMode
	setinputmode h Sensor3 LightInactive RawMode
	setinputmode h Sensor4 LowSpeed9V    RawMode -- Ultrasonic Distance sensor
	lsread h Sensor4 -- flush garbage from buffer
	i2cwrite h Sensor4 continuous_measurement_command

getSwitch :: NXTHandle -> IO (Bool)
getSwitch h = do
	iv <- getinputvalues h Sensor1
	return (scaled iv > 0)

getSound :: NXTHandle -> IO (Float)
getSound h = do
	iv <- getinputvalues h Sensor2
	return ((fromIntegral (normalized iv)) / 1023)

getLight :: NXTHandle -> IO (Float)
getLight h = do
	iv <- getinputvalues h Sensor3
	return ((fromIntegral (normalized iv)) / 1023)

getDistance :: NXTHandle -> IO (Word8)
getDistance h = do
	i2cwrite h Sensor4 read_measurement_byte_0
	waitForMeasurement 10
	putStrLn "Reading"
	r <- lsread h Sensor4
	return (B.head r)
	where waitForMeasurement cd = do
		putStrLn ("Waiting for measurement " ++ (show cd))
		s <- lsgetstatus h Sensor4 -- TODO threadDelay, Timeout um Aufhänger zu vermeiden
		if 0 < s || cd < 0
			then do
				putStrLn ("LsGetStatus " ++ (show s) ++ " CD " ++ (show cd))
				return ()
			else do
				(usleep 1000)
				waitForMeasurement (cd-1)
	
{-
Sensor medium abstraction
  Doof dass die Sensoren nicht alle parallel ausgewählt werden können, sondern umgeschaltet werden müssen
  Davon ausgehen dass die Sensoren in ihren Standardports stecken
  Subsequent versionen die einen Stream generieren
  GetSwitch True/False
  GetLight Active/Inactive Value(Normalized/Scaled/1023)
  GetDistance Value
  GetVolume dB/dBA
-}