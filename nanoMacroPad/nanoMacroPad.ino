/*
 * Arduino Nano v3 sketch for scanning key pad (a 4x4 matrix of push buttons)
 * and sending out a single letter ('0' to 'F') on the serial port
 * reporting which key has been pressed.
 * 
 * This is a variation of
 *   MacroKeyPad.ino 
 *     @ https://github.com/crixodia/arduino-nano-macro-keypad/tree/master/MacroKeyPad
 * sketch from the arduino-nano-macro-keypad repository by Cristian 
 * Bastidas (crixodia) on GitHub which is itself a variation of
 * CustomKeypad.ino, one of the example sketches from the Keypad library 
 * by Mark Stanley and Alexander Brevig. Christopher Andrews (Chriss--A) 
 * maintains the library repository on GitHub
 *   @ https://github.com/Chris--A/Keypad/blob/master/examples/CustomKeypad/CustomKeypad.ino
 */

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
   {'8', '9', 'a', 'b'},
   {'c', 'd', 'e', 'f'}
};


//Creates keypad object
Keypad customKeypad = Keypad(makeKeymap(hexaKeys), rowPins, colPins, ROWS, COLS);

void setup() {
  Serial.begin(9600);
  customKeypad.setDebounceTime(40);
  customKeypad.setHoldTime(2000);
} 

void loop() {
  char customKey = customKeypad.getKey();
  if (customKey) {
    //Print de key char from hexaKeys matrix
    Serial.println(customKey);
  }
}
