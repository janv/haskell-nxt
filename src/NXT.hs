module NXT where
	
	import NXT.Codes
	import System.IO
	import Data.Char
	import Data.Word
	import Data.Bits
	
	type Message = [Word8]
	
	-- | Add the BlueTooth length header in front of a message
	btHeader :: [Word8] -> [Word8]
	btHeader msg =  lengthLSB : lengthMSB : msg
			where lengthLSB = (fromIntegral (length msg)) .&. 0xFF
			      lengthMSB = (fromIntegral (length msg)) `shiftR` 8
	
	-- | Convert a message consisting of Bytes to a character-Stream required by hPutStr
	bytesToChars :: [Word8] -> [Char]
	bytesToChars bytes = fmap (chr . fromIntegral) bytes
	
	testloop = do
		h <- openFile "/dev/tty.NXT-DevB-1" ReadWriteMode
		hSetBuffering h NoBuffering
		hPutStr h (bytesToChars (btHeader [0x00, 0x03, 0xf4, 0x01, 0xf4, 0x01]))
		hClose h