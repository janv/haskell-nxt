-- | Commands for the Ultrasonic I2C distance sensor
module NXT.Ultrasonic where

import NXT.Codes
import NXT.Commands
import NXT.Comm
import Data.Word
import qualified Data.ByteString as B

-- I2C device ID of the sensor
i2c_dev = 0x02::Word8

------------------------------------------------------------------------------
-- I2C Ultrasonic Sensor Commands
------------------------------------------------------------------------------

data I2CCommand = I2CC {
	i2cmessage :: Message,	-- ^ I2C Address, followed by Commands
	i2rx ::Word8		-- ^ RX length (expected return length)
}

-- first value is the i2c address, second value is the expected number of bytes returned
read_version               = I2CC (B.singleton 0x00) 8
read_product_id            = I2CC (B.singleton 0x08) 8
read_sensor_type           = I2CC (B.singleton 0x10) 8
read_factory_zero          = I2CC (B.singleton 0x11) 1
read_factory_scale_factor  = I2CC (B.singleton 0x12) 1
read_factory_scale_divisor = I2CC (B.singleton 0x13) 1
read_measurement_units     = I2CC (B.singleton 0x14) 7

-- value is the i2c address (all of these ops always expect to return 1 byte)
read_continuous_measurements_interval = I2CC (B.singleton 0x40) 1
read_command_state                    = I2CC (B.singleton 0x41) 1
read_measurement_byte_0               = I2CC (B.singleton 0x42) 1
read_measurement_byte_1               = I2CC (B.singleton 0x43) 1
read_measurement_byte_2               = I2CC (B.singleton 0x44) 1
read_measurement_byte_3               = I2CC (B.singleton 0x45) 1
read_measurement_byte_4               = I2CC (B.singleton 0x46) 1
read_measurement_byte_5               = I2CC (B.singleton 0x47) 1
read_measurement_byte_6               = I2CC (B.singleton 0x48) 1
read_measurement_byte_7               = I2CC (B.singleton 0x49) 1
read_actual_zero                      = I2CC (B.singleton 0x50) 1
read_actual_scale_factor              = I2CC (B.singleton 0x51) 1
read_actual_scale_divisor             = I2CC (B.singleton 0x52) 1

-- first value is the i2c address, second value is the command
off_command                         = I2CC (B.pack [0x41, 0x00]) 0
single_shot_command                 = I2CC (B.pack [0x41, 0x01]) 0
continuous_measurement_command      = I2CC (B.pack [0x41, 0x02]) 0
event_capture_command               = I2CC (B.pack [0x41, 0x03]) 0
request_warm_reset                  = I2CC (B.pack [0x41, 0x04]) 0
set_continuous_measurement_interval = I2CC (B.singleton 0x40   ) 0
set_actual_zero                     = I2CC (B.singleton 0x50   ) 0
set_actual_scale_factor             = I2CC (B.singleton 0x51   ) 0
set_actual_scale_divisor            = I2CC (B.singleton 0x52   ) 0

-- | Wrapper around 'NXT.Commands.lswrite' that accepts a 'I2CCommand' instead
--   of seperate arguments for command and Rx. Also prepends the correct I2C
--   device ID to the message.
i2cwrite :: NXTHandle -> InputPort -> I2CCommand -> IO ()
i2cwrite h port (I2CC msg rx) = lswrite h port (i2c_dev +++ msg) rx