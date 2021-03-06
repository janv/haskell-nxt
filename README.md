HASKELL-NXT
===========

Haskell-Nxt is a library to control Lego Mindstorms NXT Bricks from Haskell.

Since the nature of the communication with the Brick is stateful, the library
consists almost entirely of monads.

Haskell-NXT is my first piece of Haskell software and probably has a lot of
rough edges still so comments, tips, bug reports or patches are very much
appreciated. 

Architecture
============

I want to briefly explain the architecture of the Library here.

NXT
---
This module is used for setting up a NXTBrick and fiddling around with it.
Mainly for testing purposes

NXT.Codes
---------
Contains various Enums and Translations between NXT protocol bytecodes and
descriptive strings or haskell types.

NXT.Helpers
-----------
Various helper functions for converting bytestrings, converting little- to
big-endian integers, dissecting messages etc.

NXT.Comm
--------
Lowest level communication with NXT Bricks. Provides a wrapper around
System.IO calls to make top-level programs a little less noisy and basic
read/write commands

NXT.Commands
------------
Implementations of the most common Direct Commands in two parts each.
One function to assemble the message to send to the Brick, one action
to send the message to the Brick in the IO Monad
A custom class `Appendable` is used to more easily combine various data types
into NXT messages.

NXT.Motor
---------
High-level library on top of NXT.Commands to easily run and stop motors.

NXT.Sensor
----------
High-level library on top of NXT.Commands to easily read out the sensors.
Requires an initialization call after opening the connection to the Brick
and assumes that the sensors are plugged into their intended ports on the
brick.

NXT.Ultrasonic
--------------
Since the Ultrasonic sensor communicates with the NXT via I2C, controlling
it is a little more complicated. This module contains mainly a list of I2C
commands.


Usage Example
=============

    btBrick = NXTBrick Bluetooth "/dev/tty.NXT-DevB-1"

    main = do
    	h <- nxtOpen btBrick
    	playtone h 500 500
    	setupDefaultSensors h
    	s <- getDistance h
    	putStrLn (show s)
    	nxtClose h

This opens a NXT Brick located as a serial device at /dev/tty.NXT-DevB-1
in Bluetooth mode (as opposed to USB, which isn't implemented yet).
The brick is opened, plays a tone and reads the distance from
the distance sensor.

Usage restrictions
==================

- The library works only on POSIX systems currently.
- It was developed on Mac OSX 10.5 with GHC 6.8.3 and uses some
  GHC-extensions to the Haskell standard.
- Communication with Bricks via USB isn't implemented yet.

TODO
====

 - I2C delay
 - Implement USB interface
 - Implement the rest of the NXT Commands
 - Debugging helpers

Credits
=======

 - Reading Tony Busers Ruby-Nxt library has greatly helped understanding the
   NXT protocol, especially controlling the Ultrasonic Sensor
   <http://rubyforge.org/projects/ruby-nxt/>
 - The Lego Mindstorms Bluetooth Developer Kit nicely described the protocol
   <http://mindstorms.lego.com/Overview/NXTreme.aspx>
 - The Labview for NXT Advanced Programming Guide contained some hints that
   were missing from the Lego docs
   

Copyright (c) 2008 [Jan Varwig](http://jan.varwig.org/), released under the MIT license
Original and official version at <http://github.com/janv/haskell-nxt>
