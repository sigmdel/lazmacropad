# Changes to the `lazmouseandkeyinput` package

`lazmouseandkeyinput` is a required package for all versions of *lazmacropad*. The package must be modified in both Windows and Linux. In Windows only one file needs to be modified, while three files must be replaced in Linux.

In both operating systems, the modified file or files must replace the original file or files in the `$(LAZARUS)/components/mouseandkeyinput` directory. It is probably best to perform a clean build after the substitution: `Run` / `Clean up and Build ...` in the Lazarus IDE.

## 1. `keyinputintf.pas` - Windows and Linux - Issue #2

The file contains changes to the `TKeyInput.Apply` and `TKeyInput.Unapply` procedures that add a fourth shift state: `ssAltGr` corresponding to the right Alt key (often labelled Alt Gr on 105 key ISO keyboards). 

## 2. `xkeyinput.pas` - Linux only - Issue #1

The file contains changes to the `VirtualKeyToXKeySym` function to handle an additional 12 virtual keys. The table shows the corresponding physical key labels found on a US-QWERTY ANSI keyboard and on a French-AZERTY keyboard as an example of ISO keyboards for non-English languages.

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
  - The FR key labels are those found on a standard 105 AZERTY ISO French keyboard. Note that the label of left most key on the top numeric row of a French AZERTY keyboard may not show the ~ above the ².
  - `VK_OEM_8`, the 105th key not found on ANSI keyboards, is usually located to the right of a smaller left shift key on ISO keyboards.

The modified file also has the `Boolean32` correction for the type of the `is_press` parameter in the external `XTestFakeKeyEvent` function. This correction has been done to the Lazarus source code ([commit 01215457](https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/012154575832231939d58b13aa0e5a373aca531d)) by Juha Manninen.  While this was done in February of 2023, the version of the package included in Lazarus 2.2.7 FPC 3.2.3 x86_64-linux-qt5 dated August 2023 does not contain the correction. **In Lazarus 4.99 the problem is no longer present, but it was problably fixed in a previous release.**

## 3. `xmouseinput.pas` - Linux only

The file has the `Boolean32` type correction of the `is_press` parameter in the external `XTestFakeMotionEvent` function argument list. Again Juha Manninen has corrected the source code([commit 72040bb5](https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/72040bb55d927ed3c9902c5d5e342bcde38d2d23)) but this has not yet found its way to the current distributions. Note that mouse input unit is not used in this project. **In Lazarus 4.99 the problem is no longer present, but it was probably fixed in a previous release.**
