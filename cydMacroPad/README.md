# Microcontroller firmware

## Hardware

ESP32-2432S028R (aka CYD - [Cheap Yellow Display](https://github.com/witnessmenow/))
  development board:

- ESP32 microcontroller with 4MB flash memory
- 2.8 inch(320x240 pixels) TFT display 
  - ILI9341 TFT driver 
- Resistive touch screen
  - XPT2046 touch controller

## Firmware version 2

Compatible with version 2 of the Arduino Nano firmware.

### Dependencies

- [TFT_eSPI](https://github.com/Bodmer/TFT_eSPI) (version 2.5.31 and up).

- a PlatformIO compatible [fork](https://github.com/sigmdel/TFT_eTouch) of [TFT_eTouch](https://github.com/achillhasler/TFT_eTouch)
  by Achill Hasler (version 0.6.0)

### PlatformIO

The [platformio.ini](platformio.ini) configuration file takes care of installing the necessary libraries, just compile and upload to the board in the PlatformIO environment.

### Arduino IDE

The firmware can be compiled in the Arduino IDE, but the process is more involved. See the explanation on [compiling examples](https://github.com/sigmdel/TFT_eTouch/blob/master/README_TOO.md#9-examples). The procedure to follow would be the same as testing `Keypad_240x320.ino` except that a better name would be `cydMacroPad` for the directory and do not forget to rename the `main.cpp` source code to `cyMacroPad.ino`.

### Binary 

A binary, `cyhdMacroPad.bin` that can be flashed with `esptool.py` is available in [other_releases](https://github.com/sigmdel/other_releases) for those just interested in testing how the macro keypad based on the cheap yellow display works.
