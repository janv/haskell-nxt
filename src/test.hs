module Main where

import FRP.Yampa
import NXT.Yampa
import NXT.Comm

btBrick = NXTBrick Bluetooth "/dev/tty.NXT-DevB-1"

main = do
	h <- nxtOpen btBrick
	
	reactimate (mkInit h) (mkSense h) (mkActuate h) nxtSF
	
	nxtClose h