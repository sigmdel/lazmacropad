/*
 * Arduino Nano v3 sketch for scanning key pad and sending out a single letter
 * on the serial port reporting which key has been pressed.
 *
 * Required library: Keypad by Mark Stanley and Alexander Brevig.
 *   Christopher Andrews (Chriss--A) maintains the Github repository
 *     @ https://github.com/Chris--A/Keypad/blob/master/examples/CustomKeypad/CustomKeypad.ino
 *
 * This sketch is a variation of the
 *   MacroKeyPad.ino
 *     @ https://github.com/crixodia/arduino-nano-macro-keypad/tree/master/MacroKeyPad
 *     sketch from the arduino-nano-macro-keypad repository by Cristian
 *     Bastidas (crixodia) on GitHub.
 *     It is a variation of  CustomKeypad.ino in the Keypad library
 *
 * lazmacropad which will receive the serial stream can handle keypads with up to 36 keys.
 *
 * The set of character sent for all keypresses must start at '0' and form a consecutive list of
 * characters taken from '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.  This is an internal convention
 * and has nothing to do with the key top symbols displayed in lazmacropad.
 *
 *  Version 2 (2023-08-18)
 *    - A keep alive (or connection ok) message added
 */

#include <Arduino.h>  // needed in PlatformIO
#include <Keypad.h>

const byte ROWS = 4;
const byte COLS = 4;

//#define SAME_AS_SILK_SCREEN

#ifdef SAME_AS_SILK_SCREEN

// Digital pins for the keypad rows and cols
// Correspondance with silkscreen:
//    S1 -- '0'
//    S2 -- '1'
//    ...
//   S16 -- 'f'
//
//                   R1 R2 R3 R4
byte rowPins[ROWS] = {6, 7, 8, 9};
//
//                   C1 C2 C3 C4
byte colPins[COLS] = {5, 4, 3, 2};

#else

// Digital pins for the keypad rows and cols
// Correspondance with silkscreen:
//    S4 -- '0', ... S16 -- '3'
//    S3 -- '4', ... S15 -- '7'
//    S2 -- '8', ... S16 -- 'b'
//    S1 -- 'c', ... S15 -- 'f'
//                   C4 C3 C2 C1
byte rowPins[ROWS] = {2, 3, 4, 5};
//
//                   R1 R2 R3 R4
byte colPins[COLS] = {6, 7, 8, 9};

#endif


// Character for each key that will be transmitted over UART
const char hexaKeys[ROWS][COLS] = {
   {'0', '1', '2', '3'},
   {'4', '5', '6', '7'},
   {'8', '9', 'A', 'B'},
   {'C', 'D', 'E', 'F'}
};

const
  char SYN = '\x16'; // any char < \x20 would work to signal that the macro keypad is still working

#define ALIVE_DELAY 5000  // maximum time (milliseconds) between messages transmitted on serial out
                          // if the keypad has not been pressed then send SYN. By default 5 seconds.

//Creates keypad object
Keypad customKeypad = Keypad(makeKeymap(hexaKeys), rowPins, colPins, ROWS, COLS);

void setup() {
  Serial.begin(9600);
  customKeypad.setDebounceTime(40);  // adjust as needed
  customKeypad.setHoldTime(2000);    // adjust as needed
}


unsigned long keepalive = 0;

void loop() {
  char customKey = customKeypad.getKey();
  if (customKey) {
    //Print de key char from hexaKeys matrix
    Serial.println(customKey);
    keepalive = millis();
  } else if ((millis() - keepalive) > ALIVE_DELAY) {
    Serial.println(SYN);
    keepalive = millis();
  }
}
