module NXT where
	
	import NXT.Commands
	import System.IO
	import qualified Data.ByteString as B
	
	testloop = do
		h <- openFile "/dev/tty.NXT-DevB-1" ReadWriteMode
		hSetBuffering h NoBuffering
		-- B.hPut h (B.pack [6,0,128,3,244,1,244,1])
		playtone h 500 500
		-- hFlush h
		hClose h