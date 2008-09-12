-- | OP and Error codes 
module NXT.Codes where
	
	import Data.Word
	
	usb_id_vendor_lego = 0x0694
	usb_id_product_nxt = 0x0002
	usb_out_endpoint   = 0x01
	usb_in_endpoint    = 0x82
	usb_timeout        = 1000
	usb_readsize       = 64
	usb_interface      = 0

	-- sensors
	sensor_1  = 0x00
	sensor_2  = 0x01
	sensor_3  = 0x02
	sensor_4  = 0x03
        
	-- motors
	data OutputPort = MotorA | MotorB | MotorC | MotorAll
	instance Enum OutputPort where
		fromEnum MotorA   = 0x00
		fromEnum MotorB   = 0x01
		fromEnum MotorC   = 0x02
		fromEnum MotorAll = 0xFF
		toEnum   0x00     = MotorA
		toEnum   0x01     = MotorB
		toEnum   0x02     = MotorC
		toEnum   0xFF     = MotorAll
        
	-- output mode
	data OutputMode = Coast | MotorOn | Brake | Regulated
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
	instance Enum RunState where
		fromEnum RunStateIdle = 0x00
		fromEnum RampUp       = 0x10
		fromEnum Running      = 0x20
		fromEnum RampDown     = 0x40
		toEnum   0x00      = RunStateIdle
		toEnum   0x10      = RampUp      
		toEnum   0x20      = Running     
		toEnum   0x40      = RampDown    
	
	-- sensor type
	no_sensor           = 0x00
	switch              = 0x01
	temperature         = 0x02
	reflection          = 0x03
	angle               = 0x04
	light_active        = 0x05
	light_inactive      = 0x06
	sound_db            = 0x07
	sound_dba           = 0x08
	custom              = 0x09
	lowspeed            = 0x0A
	lowspeed_9v         = 0x0B
	no_of_sensor_types  = 0x0C
	
	-- sensor mode
	rawmode             = 0x00 -- report scaled value equal to raw value
	booleanmode         = 0x20 -- report scaled value as 1 true or 0 false, false if raw value > 55% of total range, true if < 45%
	transitioncntmode   = 0x40 -- report scaled value as number of transitions between true and false
	periodcountermode   = 0x60 -- report scaled value as number of transitions from false to true, then back to false
	pctfullscalemode    = 0x80 -- report scaled value as % of full scale reading for configured sensor type
	celsiusmode         = 0xA0
	fahrenheitmode      = 0xC0
	anglestepsmode      = 0xE0 -- report scaled value as count of ticks on RCX-style rotation sensor
	slopemask           = 0x1F
	modemask            = 0xE0
	
	----------------------------------------------------------------------
	-- OP CODES
	----------------------------------------------------------------------
	
	data OP_CODE = 
		-- Direct Commands
		  START_PROGRAM           
		| STOP_PROGRAM            
		| PLAY_SOUND_FILE         
		| PLAY_TONE               
		| SET_OUTPUT_STATE        
		| SET_INPUT_MODE          
		| GET_OUTPUT_STATE        
		| GET_INPUT_VALUES        
		| RESET_INPUT_SCALED_VALUE
		| MESSAGE_WRITE           
		| RESET_MOTOR_POSITION    
		| GET_BATTERY_LEVEL       
		| STOP_SOUND_PLAYBACK     
		| KEEP_ALIVE              
		| LS_GET_STATUS           
		| LS_WRITE                
		| LS_READ                 
		| GET_CURRENT_PROGRAM_NAME
		| MESSAGE_READ            
		-- System Commands        
		| OPEN_READ               
		| OPEN_WRITE              
		| READ_FILE               
		| WRITE_FILE              
		| CLOSE_HANDLE            
		| DELETE_FILE             
		| FIND_FIRST              
		| FIND_NEXT               
		| GET_FIRMWARE_VERSION    
		| OPEN_WRITE_LINEAR       
		| OPEN_READ_LINEAR          -- internal command?
		| OPEN_WRITE_DATA         
		| OPEN_APPEND_DATA        
		| BOOT                      -- USB only...
		| SET_BRICK_NAME          
		| GET_DEVICE_INFO         
		| DELETE_USER_FLASH       
		| POLL_COMMAND_LENGTH     
		| POLL_COMMAND            
		| BLUETOOTH_FACTORY_RESET   -- cannot be transmitted via bluetooth
		-- IO-Map Access          
		| REQUEST_FIRST_MODULE    
		| REQUEST_NEXT_MODULE     
		| CLOSE_MODULE_HANDLE     
		| READ_IO_MAP             
		| WRITE_IO_MAP            
		
	data CommandMode = Direct | System | Reply
	-- | Translate a Commandmode into its numeric value
	instance Enum CommandMode where
		fromEnum Direct = 0x00
		fromEnum System = 0x01
		fromEnum Reply  = 0x02
		toEnum   0x00   = Direct
		toEnum   0x80   = Direct
		toEnum   0x01   = System
		toEnum   0x81   = System
		toEnum   0x02	= Reply
	commandType :: CommandMode -> Bool -> Word8
	commandType Reply  _     = 0x02
	commandType cm     True  = fromIntegral (fromEnum cm)
	commandType cm     False = fromIntegral (fromEnum cm + 0x80)
	
	-- | Translate an Opcode into its CommandMode and numeric value
	opcode :: OP_CODE -> (CommandMode, Integer)
	opcode START_PROGRAM            = (Direct, 0x00)
	opcode STOP_PROGRAM             = (Direct, 0x01)
	opcode PLAY_SOUND_FILE          = (Direct, 0x02)
	opcode PLAY_TONE                = (Direct, 0x03)
	opcode SET_OUTPUT_STATE         = (Direct, 0x04)
	opcode SET_INPUT_MODE           = (Direct, 0x05)
	opcode GET_OUTPUT_STATE         = (Direct, 0x06)
	opcode GET_INPUT_VALUES         = (Direct, 0x07)
	opcode RESET_INPUT_SCALED_VALUE = (Direct, 0x08)
	opcode MESSAGE_WRITE            = (Direct, 0x09)
	opcode RESET_MOTOR_POSITION     = (Direct, 0x0A)
	opcode GET_BATTERY_LEVEL        = (Direct, 0x0B)
	opcode STOP_SOUND_PLAYBACK      = (Direct, 0x0C)
	opcode KEEP_ALIVE               = (Direct, 0x0D)
	opcode LS_GET_STATUS            = (Direct, 0x0E)
	opcode LS_WRITE                 = (Direct, 0x0F)
	opcode LS_READ                  = (Direct, 0x10)
	opcode GET_CURRENT_PROGRAM_NAME = (Direct, 0x11)
	opcode MESSAGE_READ             = (Direct, 0x13)
	opcode OPEN_READ                = (System, 0x80)
	opcode OPEN_WRITE               = (System, 0x81)
	opcode READ_FILE                = (System, 0x82)
	opcode WRITE_FILE               = (System, 0x83)
	opcode CLOSE_HANDLE             = (System, 0x84)
	opcode DELETE_FILE              = (System, 0x85)
	opcode FIND_FIRST               = (System, 0x86)
	opcode FIND_NEXT                = (System, 0x87)
	opcode GET_FIRMWARE_VERSION     = (System, 0x88)
	opcode OPEN_WRITE_LINEAR        = (System, 0x89)
	opcode OPEN_READ_LINEAR         = (System, 0x8A)
	opcode OPEN_WRITE_DATA          = (System, 0x8B)
	opcode OPEN_APPEND_DATA         = (System, 0x8C)
	opcode BOOT                     = (System, 0x97)
	opcode SET_BRICK_NAME           = (System, 0x98)
	opcode GET_DEVICE_INFO          = (System, 0x9B)
	opcode DELETE_USER_FLASH        = (System, 0xA0)
	opcode POLL_COMMAND_LENGTH      = (System, 0xA1)
	opcode POLL_COMMAND             = (System, 0xA2)
	opcode BLUETOOTH_FACTORY_RESET  = (System, 0xA4)
	opcode REQUEST_FIRST_MODULE     = (System, 0x90)
	opcode REQUEST_NEXT_MODULE      = (System, 0x91)
	opcode CLOSE_MODULE_HANDLE      = (System, 0x92)
	opcode READ_IO_MAP              = (System, 0x94)
	opcode WRITE_IO_MAP             = (System, 0x95)
	
	-- | Translate numeric error codes from the NXT to Strings describing the Error
	error_message :: Integer -> String
	-- Direct Commands
	error_message 0x20 = "Pending communication transaction in progress"
	error_message 0x40 = "Specified mailbox queue is empty"
	error_message 0xBD = "Request failed (i.e. specified file not found)"
	error_message 0xBE = "Unknown command opcode"
	error_message 0xBF = "Insane packet"
	error_message 0xC0 = "Data contains out-of-range values"
	error_message 0xDD = "Communication bus error"
	error_message 0xDE = "No free memory in communication buffer"
	error_message 0xDF = "Specified channel/connection is not valid"
	error_message 0xE0 = "Specified channel/connection not configured or busy"
	error_message 0xEC = "No active program"
	error_message 0xED = "Illegal size specified"
	error_message 0xEE = "Illegal mailbox queue ID specified"
	error_message 0xEF = "Attempted to access invalid field of a structure"
	error_message 0xF0 = "Bad input or output specified"
	error_message 0xFB = "Insufficient memory available"
	error_message 0xFF = "Bad arguments"
	-- System Commands
	error_message 0x81 = "No more handles"
	error_message 0x82 = "No space"
	error_message 0x83 = "No more files"
	error_message 0x84 = "End of file expected"
	error_message 0x85 = "End of file"
	error_message 0x86 = "Not a linear file"
	error_message 0x87 = "File not found"
	error_message 0x88 = "Handle all ready closed"
	error_message 0x89 = "No linear space"
	error_message 0x8A = "Undefined error"
	error_message 0x8B = "File is busy"
	error_message 0x8C = "No write buffers"
	error_message 0x8D = "Append not possible"
	error_message 0x8E = "File is full"
	error_message 0x8F = "File exists"
	error_message 0x90 = "Module not found"
	error_message 0x91 = "Out of boundry"
	error_message 0x92 = "Illegal file name"
	error_message 0x93 = "Illegal handle"