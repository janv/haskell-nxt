-- | OP and Error codes 
module NXT.Codes where

import Data.Word

-- usb_id_vendor_lego = 0x0694
-- usb_id_product_nxt = 0x0002
-- usb_out_endpoint   = 0x01
-- usb_in_endpoint    = 0x82
-- usb_timeout        = 1000
-- usb_readsize       = 64
-- usb_interface      = 0

-- motors
data OutputPort = MotorA | MotorB | MotorC deriving Show --{-| MotorAll-}
instance Enum OutputPort where
	fromEnum MotorA   = 0x00
	fromEnum MotorB   = 0x01
	fromEnum MotorC   = 0x02
	-- fromEnum MotorAll = 0xFF
	toEnum   0x00     = MotorA
	toEnum   0x01     = MotorB
	toEnum   0x02     = MotorC
	-- toEnum   0xFF     = MotorAll

-- output modes
data OutputMode = Coast | MotorOn | Brake | Regulated deriving (Show, Eq)
instance Enum OutputMode where
	fromEnum Coast     = 0x00
	fromEnum MotorOn   = 0x01
	fromEnum Brake     = 0x02
	fromEnum Regulated = 0x04
	toEnum   0x00      = Coast    
	toEnum   0x01      = MotorOn  
	toEnum   0x02      = Brake    
	toEnum   0x04      = Regulated

-- output regulation mode
data RegulationMode =
	  RegulationIdle	-- ^ disables regulation
	| MotorSpeed		-- ^ auto adjust PWM duty cycle if motor is affected by physical load
	| MotorSync		-- ^ attempt to keep rotation in sync with another motor that has this set, also involves turn ratio
	deriving (Show, Eq)
instance Enum RegulationMode where
	fromEnum  RegulationIdle = 0x00
	fromEnum  MotorSpeed     = 0x01
	fromEnum  MotorSync      = 0x02
	toEnum   0x00      = RegulationIdle
	toEnum   0x01      = MotorSpeed    
	toEnum   0x02      = MotorSync     

-- output run state
data RunState =
	  RunStateIdle	-- ^ disables power to motor
	| RampUp        -- ^ ramping to a new SPEED set-point that is greater than the current SPEED set-point
	| Running       -- ^ enables power to motor
	| RampDown      -- ^ ramping to a new SPEED set-point that is less than the current SPEED set-point
	deriving Show
instance Enum RunState where
	fromEnum RunStateIdle = 0x00
	fromEnum RampUp       = 0x10
	fromEnum Running      = 0x20
	fromEnum RampDown     = 0x40
	toEnum   0x00      = RunStateIdle
	toEnum   0x10      = RampUp      
	toEnum   0x20      = Running     
	toEnum   0x40      = RampDown    

-- sensors
data InputPort = Sensor1 | Sensor2 | Sensor3 | Sensor4 deriving Show
instance Enum InputPort where
	fromEnum Sensor1 = 0x00
	fromEnum Sensor2 = 0x01
	fromEnum Sensor3 = 0x02
	fromEnum Sensor4 = 0x03
	toEnum   0x00     = Sensor1
	toEnum   0x01     = Sensor2
	toEnum   0x02     = Sensor3
	toEnum   0x03     = Sensor4

-- sensor type
data SensorType =
	  NoSensor		-- ^ No Sensor configured
	| Switch		-- ^ NXT or RCX touch sensor
	| Temperature		-- ^ RCX temperature sensor
	| Reflection		-- ^ RCX light sensor
	| Angle			-- ^ RCX rotation sensor
	| LightActive		-- ^ NXT light sensor with floodlight enabled
	| LightInactive		-- ^ NXT light sensor with floodlight disabled
	| SoundDb		-- ^ NXT sound sensor; dB scaling
	| SoundDba		-- ^ NXT sound sensor; dBA scaling
	| Custom		-- ^ Unused
	| LowSpeed		-- ^ I2C digital sensor
	| LowSpeed9V		-- ^ I2C digital sensor; 9V power
	| NoOfSensorTypes	-- ^ Unused
	deriving Show
instance Enum SensorType where
	fromEnum NoSensor        = 0x00
	fromEnum Switch          = 0x01
	fromEnum Temperature     = 0x02
	fromEnum Reflection      = 0x03
	fromEnum Angle           = 0x04
	fromEnum LightActive     = 0x05
	fromEnum LightInactive   = 0x06
	fromEnum SoundDb         = 0x07
	fromEnum SoundDba        = 0x08
	fromEnum Custom          = 0x09
	fromEnum LowSpeed        = 0x0A
	fromEnum LowSpeed9V      = 0x0B
	fromEnum NoOfSensorTypes = 0x0C
	toEnum 0x00 = NoSensor        
	toEnum 0x01 = Switch          
	toEnum 0x02 = Temperature     
	toEnum 0x03 = Reflection      
	toEnum 0x04 = Angle           
	toEnum 0x05 = LightActive     
	toEnum 0x06 = LightInactive   
	toEnum 0x07 = SoundDb         
	toEnum 0x08 = SoundDba        
	toEnum 0x09 = Custom          
	toEnum 0x0A = LowSpeed        
	toEnum 0x0B = LowSpeed9V      
	toEnum 0x0C = NoOfSensorTypes 

-- sensor mode
data SensorMode =
	  RawMode		-- ^ report scaled value equal to raw value [0..1023]
	| BooleanMode		-- ^ report scaled value as 1 true or 0 false, false if raw value > 55% of total range, true if < 45% [0,1]
	| TransitionCntMode	-- ^ report scaled value as number of transitions between true and false [0..65535]
	| PeriodCounterMode	-- ^ report scaled value as number of transitions from false to true, then back to false [0..65535]
	| PctFullScaleMode	-- ^ report scaled value as % of full scale reading for configured sensor type [0,100]
	| CelsiusMode		-- ^ readings in 10th of a degree Celsius [-200..700]
	| FahrenheitMode	-- ^ readings in 10th of a degree Fahrenheit [-400..1580]
	| AngleStepMode		-- ^ report scaled value as count of ticks on RCX-style rotation sensor [0..65535]
	| SlopeMask
	| ModeMask
	deriving Show
instance Enum SensorMode where
	fromEnum RawMode            = 0x00
	fromEnum BooleanMode        = 0x20
	fromEnum TransitionCntMode  = 0x40
	fromEnum PeriodCounterMode  = 0x60
	fromEnum PctFullScaleMode   = 0x80
	fromEnum CelsiusMode        = 0xA0
	fromEnum FahrenheitMode     = 0xC0
	fromEnum AngleStepMode      = 0xE0
	fromEnum SlopeMask          = 0x1F
	fromEnum ModeMask           = 0xE0
	toEnum 0x00 = RawMode          
	toEnum 0x20 = BooleanMode      
	toEnum 0x40 = TransitionCntMode
	toEnum 0x60 = PeriodCounterMode
	toEnum 0x80 = PctFullScaleMode 
	toEnum 0xA0 = CelsiusMode      
	toEnum 0xC0 = FahrenheitMode   
	toEnum 0xE0 = AngleStepMode    
	toEnum 0x1F = SlopeMask        
	-- toEnum 0xE0 = ModeMask      

----------------------------------------------------------------------
-- Command modes
----------------------------------------------------------------------

data CommandMode = Direct | System | Reply
instance Enum CommandMode where
	fromEnum Direct = 0x00
	fromEnum System = 0x01
	fromEnum Reply  = 0x02
	toEnum   0x00   = Direct
	toEnum   0x80   = Direct
	toEnum   0x01   = System
	toEnum   0x81   = System
	toEnum   0x02	= Reply
-- | Combine a commandmode and a Flag indicating wether to expect a reply
--   into a Command-Word encoding both
commandType :: CommandMode -> Bool -> Word8
commandType Reply  _     = 0x02
commandType cm     True  = fromIntegral (fromEnum cm)
commandType cm     False = fromIntegral (fromEnum cm + 0x80)