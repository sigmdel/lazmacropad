# Changes to the required lazmouseandkeyinput package


The file `xkeyinput.pas` contains changes to the `VirtualKeyToXKeySym` function to handle an additional 12 virtual keys.

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

