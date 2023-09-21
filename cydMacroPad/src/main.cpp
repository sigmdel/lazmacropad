/*
  Soft macro keypad built with ESP32-2432S028R (aka CYD - Cheap Yellow Display)
  dev board with 2.8 inch(320x240 pixels) TFT display using ILI9341 driver and
  XPT2046 touch controller. See ESP32-Cheap-Yellow-Display by Brian Laugh
  (@ https://github.com/witnessmenow/) about this board.

  Two librairies are used
    TFT_eSPI (version 2.5.31) by Bodmer @ https://github.com/Bodmer/TFT_eSPI
    TFT_eTouch (version 0.6.0) by Achill Hasler @ https://github.com/achillhasler/TFT_eTouch
     modified by sigmdel @ https://github.com/sigmdel/TFT_eTouch.

  This firmware is a modified version of TFT_eSPI/examples/320x240/Keypad_240x320.

  See cydMacroPad in the examples/TFT_eSPI directory of the 
  https://github.com/sigmdel/TFT_eTouch repository for a version of
  the source with extra debugging code.

  This is a drop-in replacement for the nanoMacroPad hardware keypad.
*/

#include <Arduino.h>
#include <SPI.h>
#include <TFT_eTouch.h>

#if !defined(SECOND_SPI_PORT)
  #error Define ESP32_2432S028R in platformio.ini
#endif

//SPIClass hSPI(HSPI);
TFT_eSPI tft;
SPIClass hSPI(HSPI);
TFT_eTouch<TFT_eSPI> touch(tft, TFT_ETOUCH_CS, TFT_ETOUCH_PIRQ, hSPI);

// Keypad start position, key sizes and spacing
#define KEY_X 40 // x coordinate of the centre of the first key
#define KEY_Y 30 // y x coordinate of the centre of the first key
#define KEY_W 72 // key rectangle width
#define KEY_H 52 // key rectangle height
#define KEY_SPACING_X 8 // horizontal gap between key rectangles
#define KEY_SPACING_Y 8 // vertical gap betwenn key rectangles
#define KEY_TEXTSIZE 1  // font size multiplier

// Dimension of grid of key rectangles
//
// horizontal: 4*72 + 3*8 = 312 leaving 8 pixels for left and right edge borders
//               left edge = (40 - 72/2)  = 4

//   vertical: 4*52 + 3*8 = 232 leaving 8 pixels for top and bottom edge borders
//               top edge = (30 - 52/2)  = 4

// Using two fonts since numbers are nice when bold
#define LABEL1_FONT &FreeSansOblique12pt7b // Key label font 1
#define LABEL2_FONT &FreeSansBold12pt7b    // Key label font 2

#define KEYCOUNT 16 // Create 16 keys for the keypad

char keyLabel[KEYCOUNT][2] = {
  "0", "1", "2", "3",
  "4", "5", "6", "7",
  "8", "9", "A", "B",
  "C", "D", "E", "F"
};

uint16_t keyColor[KEYCOUNT] = {
  TFT_BLUE, TFT_BLUE, TFT_BLUE, TFT_BLUE,
  TFT_BLUE, TFT_BLUE, TFT_BLUE, TFT_BLUE,
  TFT_BLUE, TFT_BLUE, TFT_BLUE, TFT_BLUE,
  TFT_BLUE, TFT_BLUE, TFT_BLUE, TFT_BLUE
};

// Invoke the TFT_eSPI button class and create all the button objects
TFT_eSPI_Button key[KEYCOUNT];

//------------------------------------------------------------------------------------------

void drawKeypad() {
  tft.setFreeFont(LABEL2_FONT);
  // Draw the keys
  for (uint8_t row = 0; row < 4; row++) {
    for (uint8_t col = 0; col < 4; col++) {
      uint8_t b = col + row * 4;

      key[b].initButton(&tft, KEY_X + col * (KEY_W + KEY_SPACING_X),
                        KEY_Y + row * (KEY_H + KEY_SPACING_Y), // x, y, w, h, outline, fill, text
                        KEY_W, KEY_H, TFT_WHITE, keyColor[b], TFT_WHITE,
                        keyLabel[b], KEY_TEXTSIZE);
      key[b].drawButton();
    }
  }
}

//------------------------------------------------------------------------------------------

#define BPIN 22   // Active buzzer + to BPIN and - to GND, half of CN1 connector
#define BDELAY 3  // Length of key press buzzer sound in ms

void beep(void) {
 #ifdef BPIN
  digitalWrite(BPIN, HIGH);
  delay(BDELAY);
  digitalWrite(BPIN, LOW);
 #endif
}

//------------------------------------------------------------------------------------------

void setup() {
  // Start serial port. Under normal operation it should not be used to output anything
  // except macro messages in the loop().
  Serial.begin(9600);

  delay(5000);  // enough time to open a terminal to see serial output when debugging

  // start touch SPI
  hSPI.begin(TFT_ETOUCH_SCK, TFT_ETOUCH_MISO, TFT_ETOUCH_MOSI, TFT_ETOUCH_CS);

  // Initialise the TFT screen driver and touch controller
  tft.begin();

  touch.init();

  #define CALIBRATION_FILE "/TFT_eTouch.cal"

    // debug aid: erase the calibration file from the file system
    #if 0 // Set to 1 to erase the file
      SPIFFS.remove(CALIBRATION_FILE);
        // Must change to "#if 0" and build and upload the firmware
        // to stop erasing the calibration file.
    #endif

    // debug aid: write the default calibration to the file system
    #if 0 // Set to 1 to write the file
      touch.setCalibration(TOUCH_DEFAULT_CALIBRATION);
      touch.writeCalibration(CALIBRATION_FILE);
        // Must change to "#if 0" and build and upload the firmware
        // to stop rewritting the calibration file.
    #endif

  #if 0
    // test setting sensitivity
    touch.setRXPlate(100);         // default  333
    touch.setRZThreshold(100*2);   // default 1000
  #endif

  if (!touch.readCalibration(CALIBRATION_FILE)) {
    #ifdef TOUCH_USE_USER_CALIBRATION
      TFT_eTouchBase::Calibation cal;
      touch.getUserCalibration(cal, 4);
      touch.setCalibration(cal);
      touch.writeCalibration(CALIBRATION_FILE); // save the new calibration

      // show result on display
      tft.fillScreen(TFT_BLACK);
      tft.setTextColor(TFT_WHITE, TFT_BLACK);
      String CalStr("{");
      CalStr += cal.x0;
      CalStr += ", ";
      CalStr += cal.x1;
      CalStr += ", ";
      CalStr += cal.y0;
      CalStr += ", ";
      CalStr += cal.y1;
      CalStr += ", ";
      CalStr += cal.rel_rotation;
      CalStr += "}";

      int cp = tft.width();
      if (tft.height() < tft.width())
        cp = tft.height();
      cp = cp / 2;
      tft.setTextDatum(MC_DATUM);
      tft.drawString("Calibration", cp, cp - 32*tft.textsize, 2);
      tft.drawString(CalStr, cp, cp, 2);
      tft.drawString("Touch screen", cp, cp + 32*tft.textsize, 2);
      int16_t x, y; // screen coordinates
      while (!touch.getXY(x, y)) delay(50);
    #else
      //Serial.printf("Calibration file %s not found. Using default calibration.\n", CALIBRATION_FILE);
    #endif
  }

  // Set the rotation
  // Landscape mode with ESP32 on left and USB connector on right when display is on top.
  tft.setRotation(1);

  // Clear the screen
  tft.fillScreen(TFT_DARKGREY);

  // Draw keypad
  drawKeypad();

#ifdef BPIN
  pinMode(BPIN, OUTPUT);
  digitalWrite(BPIN, LOW);
#endif
}

//------------------------------------------------------------------------------------------

const
  char SYN = '\x16'; // any char < \x20 would work to signal that the macro keypad is still working

#define ALIVE_DELAY 5000  // maximum time (milliseconds) between messages transmitted on serial out
                          // if the keypad has not been pressed then send SYN. By default 5 seconds.

unsigned long keepalive = 0;

void loop(void) {
  static uint32_t last_touch = 0;

  if (last_touch + touch.getMeasureWait() > millis()) return;  // wait 5 ms
  last_touch = millis();

  int16_t x, y; // screen coordinates
  bool pressed = touch.getXY(x, y);
  // Check if any key coordinate boxes contain the touch coordinates
  for (uint8_t b = 0; b < KEYCOUNT; b++) {
    if (pressed && key[b].contains(x, y)) {
      key[b].press(true);  // tell the button it is pressed
    } else {
      key[b].press(false);  // tell the button it is NOT pressed
    }
  }

  // Check if any key has changed state
  for (uint8_t b = 0; b < KEYCOUNT; b++) {
    if (key[b].justReleased()) {
+      tft.setFreeFont(LABEL2_FONT);
      key[b].drawButton();     // draw normal
      //Serial.printf("button %s released\n", keyLabel[b]);
      Serial.println(keyLabel[b]);
      beep();
    } else if (key[b].justPressed()) {
      tft.setFreeFont(LABEL1_FONT);
      key[b].drawButton(true);  // draw invert
    }
  }
  #ifdef ESP32_2432S028R
    delay(75);
  #else
    delay(10); // UI debouncing
  #endif
  if ((millis() - keepalive) > ALIVE_DELAY) {
    Serial.println(SYN);
    keepalive = millis();
  }
}
