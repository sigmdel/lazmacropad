# lazmacropad
**Version 0.2.0**

A rudimentary macro keypad based on the Arduino Nano and a simple Free Pascal/Lazarus program that pastes macros into the currently focused desktop application.

Currently the Free Pascal/Lazarus program has been tested and found to run on Windows 10 and Linux (Mint 20.1) only.

![screenshot](images/screenshot_0_1_0.jpg)

**Table of Content**
<!-- TOC -->

- [1. Hardware](#1-hardware)
- [2. Theory of Operation](#2-theory-of-operation)
- [3. Source Code and Prerequisites](#3-source-code-and-prerequisites)
  - [3.1. Linux Requirements](#31-linux-requirements)
  - [3.2. Windows Requirements](#32-windows-requirements)
- [4. Acknowledgment](#4-acknowledgment)
- [5. License](#5-license)

<!-- /TOC -->
## 1. Hardware

Only three items are needed to implement a basic macro keypad:

  1. A switch matrix. The program assumes that the key pad is composed of 16 switches arranged in a 4 x 4 matrix.

  2. An Arduino Nano R3. This is an older, smaller, Arduino board based on the ATmega328P microcontroller. It has a Mini-B USB socket, but some clones come with a Micro or Type-C USB connector.

  3. An appropriate USB cable to connect the Nano to a desktop or portable computer.

Basically, any development board with 8 digital I/O pins and a serial port could be used, but the Nano and the readily available 4x4 tactile push button matrix shown below are very easily connected to 
each other.

![hardware](images/macrokeypad.jpg)

Note that the Nano is upside down (microcontroller is on the hidden side of the board) so that the USB cable will go off to the right.

## 2. Theory of Operation

```
4 rows x 4 columns key pad
+-------------- +                   +------+                +----------------------------------------+
| o   o   o   o |                   | Nano |                |  Desktop                               |
| o   o   o   o | <---------------> |  v3  |                |                                        |
| o   o   o   o |     eight I/O     |      | <-- serial --> | <---> Lazarus service --> Application  | 
| o   o   o   o | <---------------> |      |                |                                        |
+-------------- +                   +------+                +----------------------------------------+
```               

The Nano continuously scans the key pad. When it detects that a key has been pressed, it sends a corresponding single letter string or *message* (i.e. '0', '1' or 'f') to the desktop or portable over the serial (USB) connection. 

The Lazarus/Free Pascal program translates macro key *messages* from the Nano into strings or macros that are sent to whichever application is active on the desktop. Each macro is copied to the system clipboard and then a Ctrl+V is injected into the keyboard event queue of the active application to paste the content of the clipboard.

The program also allows for editing, saving and loading macro definitions to suit any number of applications on the desktop.

## 3. Source Code and Prerequisites

[nanoMacroPad.ino](nanoMacroPad/nanoMacroPad.ino) contains the Arduino sketch that runs on the Nano. It requires the [Keypad library](https://playground.arduino.cc/Code/Keypad/) ([GitHub repository](https://github.com/Chris--A/Keypad)) by Mark Stanley and Alexander Brevig. It can be installed with the Arduino IDE library manager.

The source code of the object pascal program consists of the following files:

    lazmacropad.lpi
    lazmacropad.lpr
    lazmacropad.res
    lazmacropad.ico
    main.pas, main.lfm 
    about.pas, about.lfm 
    layout.pas, layout.lfm
    params.pas

The [`MouseAndKeyInput`](https://wiki.lazarus.freepascal.org/MouseAndKeyInput) unit is used to generate the Ctrl-V keyboard event which is the usual paste keyboard shortcut. The unit is found in the `lazmouseandkeyinput.lpk` non-visual package. The package found in the `$(LAZARUS)/components/mouseandkeyinput` directory. Load the package file into the Lazarus IDE and compile it. Add `lazmouseandkeyinput` to the `Required Packages` in the `Project Inspector` window (which is opened from the `Project` menu in the IDE).

### 3.1. Linux Requirements

The source was compiled in Lazarus (version 2.3.0 / Free Pascal compiler 3.3.1) on a Linux Mint 20.1 desktop and tested on the same machine. 

The `libxtst` library is required. In recent versions of **Debian** the package containing this library is called `libxtst6`. In Linux Mint 20.1, the package was installed but the library could not be found until a symbolic link was added. 

```bash
$ cd /usr/lib/x86_64-linux-gnu
$ sudo ln -s libXtst.so.6.1.0 libXtst.so
```

Check if that is necessary with the following commands.

```bash
$ sudo updatedb
$ locate libXtst
```

### 3.2. Windows Requirements

The source was compiled in Lazarus (version 2.2.4 / Free Pascal compiler 3.2.2) on a Windows 10 desktop and tested on the same machine. 

No additional libraries are required in Windows.

It is not possible to test for a lost serial connection in Windows.

## 4. Acknowledgment

There is no shortage of information on all sorts of more or less sophisticated macro key pads. 
The Brian Lough video, [The Simplest DIMY Macro Keypad with Arduino](https://www.youtube.com/watch?v=ORujXGDqG_I&ab_channel=BrianLough) was the initial inspiration, but that project is based on an Arduino Pro Micro. The Cristian Bastidas (crixodia) [arduino-nano-macro-keypad](https://github.com/crixodia/arduino-nano-macro-keypad) GitHub repository showed how to create a similarly simple project using a Python script on the desktop.


## 5. License

The **BSD Zero Clause** ([SPDX](https://spdx.dev/): [0BSD](https://spdx.org/licenses/0BSD.html)) licence applies to the original code in this repository.
