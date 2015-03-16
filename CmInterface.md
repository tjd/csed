# Introduction #

cm-interface is a software API for the Command Module for iRobot's Create.

# Software requirements #

As this is only source code, a cross-compiler is required to compile and download it to the Command Module.

Under Windows, the software that comes with the Command Module is sufficient.

Under Linux, you need to install the following packages:
  * avrdude (for downloading the code to the device)
  * gcc-avr (for compiling)

I'm not sure what software you'd need to compile it under MacOS.

# Sample program #

Included with this library is a sample program that tries to escape from a room by following a wall, called `"wall-follow.c"`.

To compile it, use the included makefile and enter the following on the command-line:

`make`

To download it to the Command Module, plug it into an available USB port and type the following into the command-line:

`make program`

(as a shortcut, you can just type `make program` to compile it _and_ download it to the device)

# Using this library #

To use this library, add the files "cm-interface.c" and "oi.h" to your project, and add the following _#include_ line to the top of your source code:

`#include "cm-interface.c"`

During your program's initialization, it is imperative that you call cm\_init() before trying to use any other functions, as it sets up the communication channel between you and the robot.

When your code is ready to start using the robot, it is recommended that you turn it on with a call to cm\_power\_on() (to ensure the robot is, in fact, on) and then cm\_start().
(it is also recommended to slow the communication channel down to a baud rate of 28800 bits per second so no data is lost)

After that, you can ask for all sensor data to be updated with a call to cm\_update\_sensors(), or a specific on with cm\_update\_sensor(sensor\_code), or you can tell the robot to move around with the cm\_drive(speed, radius) or cm\_direct\_drive(right\_speed, left\_speed) functions.

Most of the functions in this library are based on the Open Interface documentation, so for full reference of the commands, please read that document available here:
[Create Open Interface reference](http://www.irobot.com/filelibrary/pdfs/hrd/create/Create%20Open%20Interface_v2.pdf)

# Things to do #

  * Write complete documentation on this API.
  * Parse out all the sensor data and provide a simple way to read it (maybe use "getters" with a little bit of intelligence?)
  * Add a little bit more intelligence to the sample program so it can tell when it gets stuck driving around an obstacle