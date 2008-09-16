module NXT.Sensor where

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