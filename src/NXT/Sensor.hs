module NXT.Sensor where

import NXT.Commands
import NXT.Codes
import System.IO
import Data.List
import Data.Int
import Data.Word

setupDefaultSensors :: Handle -> IO ()
setupDefaultSensors h = do
	setinputmode h Sensor1 Switch      BooleanMode
	setinputmode h Sensor2 SoundDb     RawMode
	setinputmode h Sensor3 LightInactive RawMode
	setinputmode h Sensor4 LowSpeed9V  BooleanMode	-- Ultrasonic Distance sensor

getSwitch :: Handle -> IO (Bool)
getSwitch h = do
	iv <- getinputvalues h Sensor1
	return (scaled iv > 0)






getLight :: Handle -> IO (Float)
getLight h = do
	iv <- getinputvalues h Sensor3
	return ((fromIntegral (normalized iv)) / 1023)
	
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