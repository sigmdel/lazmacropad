# Microcontroller firmware
## Firmware version 2

Starting with version 0.9.0 of *lazmacropad*, the Arduino Nano must be running firmware version 2.

### Dependencies

 [Keypad library](https://playground.arduino.cc/Code/Keypad/) ([GitHub repository](https://github.com/Chris--A/Keypad)) by Mark Stanley and Alexander Brevig. It can be installed with the Arduino IDE library manager.

### Source

[nanoMacroPad.ino](nanoMacroPad.ino) is the Arduino sketch that must be uploaded to the  Nano. 

### PlatformIO

A [platformio.ini](platformio.ini) configuration file is included to compile and upload the firmware with the PlatformIO environment if preferred. The configuration file takes care of installing the Keypad library.
