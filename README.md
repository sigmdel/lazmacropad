# *lazmacropad*
**Macro Keypad version 0.6.0**

A rudimentary macro keypad based on the Arduino Nano and a simple Free Pascal/Lazarus program that pastes macros into the currently focused desktop application.

Currently this Free Pascal/Lazarus tray icon program has been tested and found to run on Linux (Mint 20.1). 

> There are cosmetic problems with the macro definition screen when running on Windows 10. While there is a work around, it will not be published until tests determine if the problem is resolved in a newer edition of Free Pascal/Lazarus. 

![screenshot](images/screenshot_0_6_0.jpg)

**Table of Content**
<!-- TOC -->

- [1. Hardware](#1-hardware)
- [2. Theory of Operation](#2-theory-of-operation)
- [3. Source Code and Prerequisites](#3-source-code-and-prerequisites)
  - [3.1. Linux Requirements](#31-linux-requirements)
  - [3.2. Windows Requirements](#32-windows-requirements)
- [4. Macros](#4-macros)
- [5. Ctrl+V vs Shift+Insert](#5-ctrlv-vs-shiftinsert)
- [6. Tray Application](#6-tray-application)
- [7. Acknowledgment](#7-acknowledgment)
- [8. License](#8-license)

<!-- /TOC -->
## 1. Hardware

Only three items are needed to implement a basic macro keypad:

  1. A switch matrix. The program assumes that the key pad is composed of 16 switches arranged in a 4 x 4 matrix.

  2. An Arduino Nano R3. This is an older, smaller, Arduino board based on the ATmega328P microcontroller. It has a Mini-B USB socket, but some clones come with a Micro or Type-C USB connector.

  3. An appropriate USB cable to connect the Nano to a desktop or portable computer.

Basically, any development board with 8 digital I/O pins and a serial port could be used, but the Nano and the readily available 4x4 tactile push button matrix shown below are very easily connected to 
each other.

![hardware](images/macrokeypad.jpg)

Note that the Nano is upside down (microcontroller is on the hidden side of the board) so that the USB cable will go off to the right. The Nano could be connected facing in the other direction by flipping it right side up if it is more convenient to orient the cable towards the left. The character transmitted over the serial port as of each key is pressed and the mapping of the Nano I/O pins should be modified in the Arduino sketch running on the Nano. 

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

The Lazarus/Free Pascal program translates macro key *messages* from the Nano into strings or macros that are sent to whichever application is active on the desktop. Each macro is copied to the system clipboard and then a Ctrl+V or Shift+Insert key combination is injected into the keyboard event queue of the active application to paste the content of the clipboard.

The program also allows for editing, saving and loading macro definitions to suit any number of applications on the desktop.

## 3. Source Code and Prerequisites

[nanoMacroPad.ino](nanoMacroPad/nanoMacroPad.ino) contains the Arduino sketch that runs on the Nano. It requires the [Keypad library](https://playground.arduino.cc/Code/Keypad/) ([GitHub repository](https://github.com/Chris--A/Keypad)) by Mark Stanley and Alexander Brevig. It can be installed with the Arduino IDE library manager.

The source code of the *lazmacropad* object pascal program is in the root directory of the repository. The [images](images/) directory contains the two images shown on this page and the image used as a background of the keyboard layout window. None of these are required to compile the program; the image bitmap is stored in the `keymap.lfm` form resource file. 

The [`MouseAndKeyInput`](https://wiki.lazarus.freepascal.org/MouseAndKeyInput) unit is used to generate the desired paste keyboard shortcut. The unit is found in the `lazmouseandkeyinput.lpk` non-visual package. The package found in the `$(LAZARUS)/components/mouseandkeyinput` directory. Load the package file into the Lazarus IDE and compile it. Add `lazmouseandkeyinput` to the `Required Packages` in the `Project Inspector` window (which is opened from the `Project` menu in the IDE).

### 3.1. Linux Requirements

The source was compiled in Lazarus (version 2.3.0 / Free Pascal compiler 3.3.1) on a Linux Mint 20.1 desktop and tested on the same machine. 

The `Xtst` library is required. In recent versions of **Debian** the package containing this library is called `libxtst6`. It was already installed in Linux Mint 20.1, although the library could not be found because of a missing symbolic link. If necessary, install the library in the usual fashion.

```bash
$ apt install libxtst6
```

Check if the needed `libXtst.so` symbolic link is in place.

```bash
$ locate libXtst
```

If `/usr/lib/x86_64-linux-gnu/libXtst.so` is not listed by the last command add the symbolic link and update the library database.

```bash
$ cd /usr/lib/x86_64-linux-gnu
$ sudo ln -s libXtst.so.6.1.0 libXtst.so
$ sudo updatedb
```

> Note: it appears some have been installing the development package `libxtst-dev` in a bid to add the missing symbolic link, but there is no need for that. Also, it was not necessary to install the symbolic link in Mint 21.

Chances are the program will work. Unfortunately, in some older distributions, an unending sequence of key down events could be generated when a macro is pasted. Those virtual events will continue until a key on the physical keyboard is pressed. As it happens, Mint 20.1 released in January 2021 (an LTS version valid until 2025) is one those *older* distributions. The solution to that problem is to change the type of the `is_press` argument in the `XTestFakeKeyEvent` function to `Boolean32` instead of `Boolean`. The function is in the file `$(lazarus)/components/mouseandkeyinput/xkeyinput.pas`. The problem and solution were provided by bytesbites in an August 16, 2016 [forum post](https://forum.lazarus.freepascal.org/index.php/topic,33719.msg218852.html#msg218852). See issues [27819](https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/27819) and [39964](https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/39964) in the Lazarus Gitlab repository.


### 3.2. Windows Requirements

The source was compiled in Lazarus (version 2.2.4 / Free Pascal compiler 3.2.2) on a Windows 10 desktop and tested on the same machine. Currently, not all issues with the Windows version have been resolved.

No additional libraries are required in Windows.

Note: The Windows version of the `Serial` unit does have a serial connection status variable. Consequently, there will be a log message about a lost connection when running in Windows.

## 4. Macros

Macros are just (UTF8) strings, but 3 escape sequences are defined:

  -  '\n' will be converted to #13 (RETURN)
  -  '\t' will be converted to #9  (TAB)
  -  '\\\\' will be converted to '\\' 

Appending a \n sequence to a macro to be used in the terminal will mean that it will be executed as soon as the corresponding key is pressed. This could be used to start a program. Be careful though, some programs such as the Arduino IDE can grab the serial port and, as a result, *lazmacropad* freezes.

## 5. Ctrl+V vs Shift+Insert

Two cut-and-paste mechanisms are used in X clients (and in Wayland also?): 

  1. the *clipboard* where the Ctlr+C keyboard short cut explicitly copies selected text (images, etc.) to a buffer and Ctlr+V pastes the content of the buffer at the cursor
  2. the *primary selection* where there is no buffer, the current selection is pasted with a click of the middle mouse button. The Shift+Insert shortcut can be used instead of a middle mouse button click to paste the selection.

While these two mechanisms are nominally independent, many applications, such as Geany, VSCodium (and probably VS Code), GNote, LibreOffice Writer and the Lazarus IDE editor, synchronize the two selections and paste selected text with either Ctrl+V or Shift+Insert. Other applications, notably the Mate Terminal, will only work with the primary selection, which makes sense since Ctrl+C is used to cancel a running program. 

When the Shift+Insert is used as the paste command, macros will be copied to the clipboard and the primary selection so that *lazmacropad* will behave much as Geany, VSCodium etc. Starting with version 0.3.2 this is done with methods from the LCL TClipboard class so presumably it should work with all Linux widgesets that support the two cut-and-paste mechanism. Warning: this has only been tested with the GTK2 widgetset.

In Windows, only the Ctrl+V paste command is used, no matter which value is assigned to the paste command parameter. 

## 6. Tray Application

As of version 0.6.0, *lazmacropad* is a tray application. This required a major reorganization of the source code. The three different tabs of the original program where moved to windows that can be displayed independently as illustrated in the screenshot shown above.

## 7. Acknowledgment

There is no shortage of information on all sorts of more or less sophisticated macro key pads. The Brian Lough video, [The Simplest DIY Macro Keypad with Arduino](https://www.youtube.com/watch?v=ORujXGDqG_I&ab_channel=BrianLough) and corresponding [GitHub repository, arduino-switcheroonie,](https://github.com/witnessmenow/arduino-switcheroonie) stands out for its simplicity, but that project is based on an Arduino Pro Micro. The Cristian Bastidas (crixodia) [arduino-nano-macro-keypad](https://github.com/crixodia/arduino-nano-macro-keypad) GitHub repository showed how to achieve something very similar  with a Nano and a Python script on the desktop. This later project is clearly the model for *lazmacropad*.

## 8. Licence

Copyright 2022, Michel Deslierres, no rights reserved.

In those jurisdictions where releasing a work into the public domain may be a problem, the **BSD Zero Clause Licence** ([SPDX](https://spdx.dev/): [0BSD](https://spdx.org/licenses/0BSD.html)) applies.
