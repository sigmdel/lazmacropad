# Changes to the required lazmouseandkeyinput package

## `xkeyinput.pas` 

The file contains changes to the `VirtualKeyToXKeySym` function to handle an additional 12 virtual keys.

| Virtual key | US key | FR key | 
|   ---   | :---:  | :---: |
| VK_OEM_1      | ; : |  ; . |
| VK_OEM_PLUS   | = + |  = + | 
| VK_OEM_COMMA  | , < |  , ? | 
| VK_OEM_MINUS  | - _ |  - 6 | 
| VK_OEM_PERIOD | . > |  ; . | 
| VK_OEM_2      | / ? |  : / | 
| VK_OEM_3      | ` ~ |  ² ~ | 
| VK_OEM_4      | [ { |  ( 5 | 
| VK_OEM_5      | \ | |  _ 8 | 
| VK_OEM_6      | ] } |  ) ° | 
| VK_OEM_7      | ' " |  " 3 | 
| VK_OEM_8      |     |  < > | 

  - The US key labels are those found on a standard 104 QWERTY ANSI US English keyboard.
  - The FR key labels are those found on a standard 105 AZERTY ISO French keyboard.
  - `VK_OEM_8`, the 105th key not found on the ANSI keyboard, is usually located to the right of a smaller left shift key on ISO keyboards.

The modified file should replace the original file in the `$(LAZARUS)/components/mouseandkeyinput` directory. Note that the modified file also has the `Boolean32` correction for the type of the `is_press` parameter of the external `XTestFakeKeyEvent` function.

## `keyinputintf.pas`

The file contains changes to the `TKeyInput.Apply` and `TKeyInput.Unapply` procedures that adds a fourth shift state: `ssAltGr` that corresponds to the right Alt key (often labeled Alt Gr on 105 key ISO keyboards). 

## `xmouseinput`

The file has the `Boolean32` type correction of the the `is_press` parameter in the external `XTestFakeMotionEvent` function argument list. This unit is not used in this project; the correction is included here just because the same correction was needed in the `xkeyinput.pas` file.
