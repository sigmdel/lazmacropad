unit kbdev;

{$mode ObjFPC}{$H+}

interface

{$PACKSET 1}

uses
  Classes, SysUtils, LCLType;

type
  TKbdShiftEnum = (ksShift, ksAlt, ksCtrl, ksAltGr);
  // ksShift..ksCtrl must be in the same order as ssShift..ssCtrl
  TKbdShift = set of TKbdShiftEnum;

  { TKbdEvent }

  TKbdEvent = object
  public
    Code: byte;
    Shift: TKbdShift;
    Delayms: word;
    Press: boolean;

    // Convert the key code to a key name or 2 digit hex value if the
    // name is not known.
    function CodetoStr: string;

    // Convert Shift.State to string representation.
    // If the no modifier keys specified returns empty string.
    function ShiftStateToStr: string;

    // Convert the complete event to a hex string used for
    // storage/retrieval in a text ini file
    function EventToHex: string;

    // Convert the complete event to a string.
    function EventToStr: string;

    // Parses the value string starting at position index to
    // set the kbd event fields.
    procedure HexToEvent(const value: string; var index: integer);
  end;

  TKbdMacro = array of TKbdEvent;

  // String representation of shortcuts macro
  // This can be modified at will, the string representation is
  // never converted back to a TKbdMacro
  function KbdMacroToStr(value: TKbdMacro): string;

  // Hex representation of shortcuts macro used
  // for saving/loading from macro definition file
  function KbdMacroToHex(value: TKbdMacro): string;
  function HexToKbdMacro(const value: string): TKbdMacro;

  // test of validity
  function TestKbdMacro(value: TKbdMacro; var index: integer): integer;

procedure AssignKeyNamesCodes(strings: TStrings);

function KeyCodeFind(key: integer; var index: integer): boolean;

// equality operator for TKbdEvent
operator = (const ke1, ke2: TKbdEvent): boolean;

implementation

uses
  LCLProc, LCLStrConsts;

// TKbdEvent operator
operator = (const ke1, ke2: TKbdEvent): boolean;
begin
  result := (ke1.Code = ke2.Code)
     and (ke1.Shift = ke2.Shift)
     and (ke1.Delayms = ke2.Delayms)
     and (ke1.Press = ke2.Press);
end;

const
  UpArrow   = #$E2+#$86+#$91; // UTF-8 encoding of U+2191 UPWARD ARROW
  DownArrow = #$E2+#$86+#$93; // UTF-8 encoding of U+2193 DOWNWARD ARROW

const
  // MS documentation:
  // https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  // keys ordered by code, so a binary search can be used on the code
  KEYCOUNT = 129;
  KeyCodesAndStrings: array[0..KEYCOUNT-1] of record name: string; code: byte end = (
    (name: 'Mouse_Left'; code: $1),  // VK_LBUTTON
    (name: 'Mouse_Right'; code: $2),  // VK_RBUTTON
    (name: 'Cancel'; code: $3),  // VK_CANCEL
    (name: 'Mouse_Middle'; code: $4),  // VK_MBUTTON
    (name: 'Mouse_X1'; code: $5),  // VK_XBUTTON1
    (name: 'Mouse_X2'; code: $6),  // VK_XBUTTON2
    (name: 'Backspace'; code: $8),  // VK_BACK
    (name: 'Tab'; code: $9),  // VK_TAB
    (name: 'NumClear'; code: $c),  // VK_CLEAR
    (name: 'Enter'; code: $d),  // VK_RETURN
    (name: 'Break'; code: $13),  // VK_PAUSE
    (name: 'CapsLock'; code: $14),  // VK_CAPITAL
    (name: 'IME_Kana'; code: $15),  // VK_KANA
    (name: 'IME_Junja'; code: $17),  // VK_JUNJA
    (name: 'IME_final'; code: $18),  // VK_FINAL
    (name: 'IME_Hanja'; code: $19),  // VK_HANJA
    (name: 'Esc'; code: $1b),  // VK_ESCAPE
    (name: 'IME_convert'; code: $1c),  // VK_CONVERT
    (name: 'IME_nonconvert'; code: $1d),  // VK_NONCONVERT
    (name: 'IME_accept'; code: $1e),  // VK_ACCEPT
    (name: 'IME_mode_change'; code: $1f),  // VK_MODECHANGE
    (name: 'Space'; code: $20),  // VK_SPACE
    (name: 'PgUp'; code: $21),  // VK_PRIOR
    (name: 'PgDown'; code: $22),  // VK_NEXT
    (name: 'End'; code: $23),  // VK_END
    (name: 'Home'; code: $24),  // VK_HOME
    (name: 'Left'; code: $25),  // VK_LEFT
    (name: 'Up'; code: $26),  // VK_UP
    (name: 'Right'; code: $27),  // VK_RIGHT
    (name: 'Down'; code: $28),  // VK_DOWN
    (name: 'Select'; code: $29),  // VK_SELECT
    (name: 'Print'; code: $2a),  // VK_PRINT
    (name: 'Execute'; code: $2b),  // VK_EXECUTE
    (name: 'PrintScreen'; code: $2c),  // VK_SNAPSHOT
    (name: 'Ins'; code: $2d),  // VK_INSERT
    (name: 'Del'; code: $2e),  // VK_DELETE
    (name: 'Help'; code: $2f),  // VK_HELP
    (name: '0'; code: $30),  // VK_0
    (name: '1'; code: $31),  // VK_1 ...
    (name: '2'; code: $32),
    (name: '3'; code: $33),
    (name: '4'; code: $34),
    (name: '5'; code: $35),
    (name: '6'; code: $36),
    (name: '7'; code: $37),
    (name: '8'; code: $38),
    (name: '9'; code: $39),
    (name: 'A'; code: $41),
    (name: 'B'; code: $42),
    (name: 'C'; code: $43),
    (name: 'D'; code: $44),
    (name: 'E'; code: $45),
    (name: 'F'; code: $46),
    (name: 'G'; code: $47),
    (name: 'H'; code: $48),
    (name: 'I'; code: $49),
    (name: 'J'; code: $4A),
    (name: 'K'; code: $4B),
    (name: 'L'; code: $4C),
    (name: 'M'; code: $4D),
    (name: 'N'; code: $4E),
    (name: 'O'; code: $4F),
    (name: 'P'; code: $50),
    (name: 'Q'; code: $51),
    (name: 'R'; code: $52),
    (name: 'S'; code: $53),
    (name: 'T'; code: $54),
    (name: 'U'; code: $55),
    (name: 'V'; code: $56),
    (name: 'W'; code: $57),
    (name: 'X'; code: $58),  // ...
    (name: 'Y'; code: $59),  // VK_Y
    (name: 'Z'; code: $5A),  // VK_Z
    (name: 'PopUp'; code: $5d),  // VK_APPS
    (name: 'Sleep'; code: $5f),  // VK_SLEEP
    (name: 'Num0'; code: $60),  // VK_NUMPAD0
    (name: 'Num1'; code: $61),  // VK_NUMPAD1
    (name: 'Num2'; code: $62),  // VK_NUMPAD2
    (name: 'Num3'; code: $63),  // VK_NUMPAD3
    (name: 'Num4'; code: $64),  // VK_NUMPAD4
    (name: 'Num5'; code: $65),  // VK_NUMPAD5
    (name: 'Num6'; code: $66),  // VK_NUMPAD6
    (name: 'Num7'; code: $67),  // VK_NUMPAD7
    (name: 'Num8'; code: $68),  // VK_NUMPAD8
    (name: 'Num9'; code: $69),  // VK_NUMPAD9
    (name: 'NumMul'; code: $6a),  // VK_MULTIPLY
    (name: 'NumPlus'; code: $6b),  // VK_ADD
    (name: 'NumSepar'; code: $6c),  // VK_SEPARATOR
    (name: 'NumMinus'; code: $6d),  // VK_SUBTRACT
    (name: 'NumDot'; code: $6e),  // VK_DECIMAL
    (name: 'NumDiv'; code: $6f),  // VK_DIVIDE
    (name: 'F1'; code: $70),  // VK_F1
    (name: 'F2'; code: $71),  // VK_F2
    (name: 'F3'; code: $72),  // VK_F3
    (name: 'F4'; code: $73),  // VK_F4
    (name: 'F5'; code: $74),  // VK_F5
    (name: 'F6'; code: $75),  // VK_F6
    (name: 'F7'; code: $76),  // VK_F7
    (name: 'F8'; code: $77),  // VK_F8
    (name: 'F9'; code: $78),  // VK_F9
    (name: 'F10'; code: $79),  // VK_F10
    (name: 'F11'; code: $7a),  // VK_F11
    (name: 'F12'; code: $7b),  // VK_F12
    (name: 'F13'; code: $7c),  // VK_F13
    (name: 'F14'; code: $7d),  // VK_F14
    (name: 'F15'; code: $7e),  // VK_F15
    (name: 'F16'; code: $7f),  // VK_F16
    (name: 'F17'; code: $80),  // VK_F17
    (name: 'F18'; code: $81),  // VK_F18
    (name: 'F19'; code: $82),  // VK_F19
    (name: 'F20'; code: $83),  // VK_F20
    (name: 'F21'; code: $84),  // VK_F21
    (name: 'F22'; code: $85),  // VK_F22
    (name: 'F23'; code: $86),  // VK_F23
    (name: 'F24'; code: $87),  // VK_F24
    (name: 'NumLock'; code: $90),  // VK_NUMLOCK
    (name: 'ScrollLock'; code: $91),  // VK_SCROLL
    (name: ';'; code: $ba), // VK_OEM_1 - Can vary by keyboard, US keyboard, the ';:' key
    (name: '='; code: $bb), // VK_OEM_PLUS - For any country/region, the '=+' key
    (name: ','; code: $bc), // VK_OEM_COMMA - Can vary by keyboard, US keyboard, the ',<' key
    (name: '-'; code: $bd), // VK_OEM_MINUS - Can vary by keyboard, US keyboard, the '-_' key
    (name: '.'; code: $be), // VK_OEM_PERIOD - Can vary by keyboard, US keyboard, the '.>' key
    (name: '/'; code: $bf), // VK_OEM_2 - Can vary by keyboard, US keyboard, the '/?' key
    (name: '`'; code: $c0), // VK_OEM_3 - Can vary by keyboard, US keyboard, the '`~' key
    (name: '['; code: $db), // VK_OEM_4 - Can vary by keyboard, US keyboard, the '[{' key
    (name: '\'; code: $dc), // VK_OEM_5 - Can vary by keyboard, US keyboard, the '\|' key
    (name: ']'; code: $dd), // VK_OEM_6 - Can vary by keyboard, US keyboard, the ']}' key
    (name: '"'; code: $de), // VK_OEM_7 - Can vary by keyboard, US keyboard, the ''"}' key
    (name: '<'; code: $df)  // VK_OEM_8 - Can vary by keyboard, 105th key not on US keyboard
  );
  { #todo -oMichel -cRefactoring : Use Classes TIdentMapEntry, IntToIdent etc. instead }

procedure AssignKeyNamesCodes(strings: TStrings);
var
 i: integer;
begin
  Strings.BeginUpdate;
  try
    strings.Clear;
    for i := 0 to KEYCOUNT-1 do
      strings.AddObject(KeyCodesAndStrings[i].name, TObject(PtrUint(KeyCodesAndStrings[i].code)));
  finally
    Strings.EndUpdate;
  end;
end;

function KeyCodeFind(key: integer; var index: integer): boolean;
var
  lo, hi: integer;
begin
  // binary search
  lo := 0;
  hi := KEYCOUNT-1;
  while hi <> lo do begin
    index := (lo + hi) div 2;
    if KeyCodesAndStrings[index].Code < key then { first half }
      lo := index+1
    else
      hi := index
  end;
  result := (key = KeyCodesAndStrings[lo].Code);
  if result then
    index := lo
  else
    index := -1;
end;

function KeyNameIndex(const name: string; var index: integer): boolean;
begin
  result := true;
  index := KEYCOUNT-1;
  repeat
    if KeyCodesAndStrings[index].Name = name then
      exit;
    dec(index);
  until index < 0;
  result := false;
end;

procedure addPart(const apart, separator: string; var value: string);
begin
  if value = '' then
    value := apart
  else
    value := value + separator + apart;
end;


function HexToKbdMacro(const value: string): TKbdMacro;
var
  i, n, count: integer;
begin
  count := strToInt('$' + copy(value, 1, 2));
  n := 1;
  i := 3;
  setlength(result, 0);
  while i < length(value) do begin
    setlength(result, n);
    result[n-1].HexToEvent(value, i);
    inc(n);
  end;
  if n-1 <> count then
    Raise Exception.CreateFmt('Expected %d events, found %d', [count, n-1]);
end;

function KbdMacroToStr(value: TKbdMacro): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to length(value)-1 do
    addPart(value[i].EventToStr, ' ', result);
end;

function KbdMacroToHex(value: TKbdMacro): string;
var
  i: integer;
begin
  result := inttohex(length(value), 2);
  for i := 0 to length(value)-1 do
    addPart(value[i].EventToHex, '', result);
end;

{ TKbdEvent}

function TKbdEvent.CodetoStr: string;
var
  index: integer;
begin
  if KeyCodeFind(Code, index) then
    result := KeyCodesAndStrings[index].Name
  else
    result := IntToHex(Code,2);
end;

//   12 3 4567 8
//   38 5 1000 0
function TKbdEvent.EventToHex: string;
begin
 result := inttohex(Code, 2);
 result := result + inttohex(byte(Shift), 1);
 result := result + inttohex(delayms, 4);
 result := result + inttohex(ord(Press),1);
end;

function TKbdEvent.EventToStr: string;
begin
  if Press then
    result := DownArrow + ShiftStateToStr + CodeToStr
  else
    result :=  UpArrow + CodeToStr + ShiftStateToStr;
  if delayms > 0 then
    result := Format('(%dms)%s', [delayms, result]);
end;

function TKbdEvent.ShiftStateToStr: string;
begin
  Result := '';
  if ksCtrl in Shift then AddPart(ifsCtrl, ',', result);
  if ksAlt in Shift then AddPart(ifsAlt, ',', result);
  if ksAltGr in Shift then AddPart('AltGr', ',', result);
  if ksShift in Shift then AddPart(ifsVK_SHIFT, ',', result);
  if result <> '' then
    result := '[' + result + ']';
end;

procedure TKbdEvent.HexToEvent(const value: string; var index: integer);
begin
  Code := 0;
  Shift := [];
  Press := false;
  DelayMs := 0;
  if index < 1 then
    index := 1;
  if index + 8 > length(value)+1 then begin
    index := length(value)+1; // signal done!
    exit;
  end;
  Code := strToInt('$' + copy(value, index, 2));
  inc(index, 2);
  Shift := TKbdShift( byte( strToInt('$' + copy(value, index, 1)) ));
  inc(index);
  DelayMs := strToInt('$' + copy(value, index, 4));
  inc(index, 4);
  Press := strToInt(copy(value, index, 1)) = 1;
  inc(index);
end;

function TestKbdMacro(value: TKbdMacro; var index: integer): integer;
var
  KeyDown: array[0..255] of boolean;
  shiftstate: set of TKbdShiftEnum;
  aResult, i: integer;

  function adjustShift(ss: TKbdShiftEnum; press: boolean): boolean;
  begin
    if press then begin
      if (ss in shiftstate) then
        aResult := -4 // already low
      else
        include(shiftstate, ss);
    end
    else begin // not press
      if (ss in shiftstate) then
        exclude(shiftstate, ss)
      else
        aResult := -5;
    end;
    result := aResult = 0;
  end;

begin
  fillchar(keydown, 256, 0);
  shiftstate := [];
  aResult := 0;
  try
    if length(value) < 1 then begin
      aResult := -1;
      exit;
    end;
    index := 0;
    while index < length(value) do begin
      if value[index].Press then begin
        if KeyDown[value[index].Code] then begin
          aResult := -2;  // key is already pressed
          exit;
        end;
        KeyDown[value[index].Code] := true;
      end
      else begin // not value[index].Press
        if KeyDown[value[index].Code] then
          KeyDown[value[index].Code] := false
        else begin
          aResult := -3;  // key is already released
          exit;
        end;
      end;
      if (ksShift in value[index].Shift) and not adjustShift(ksShift, value[index].Press) then exit;
      if (ksCtrl in value[index].Shift) and not adjustShift(ksCtrl, value[index].Press) then exit;
      if (ksAlt in value[index].Shift) and not adjustShift(ksAlt, value[index].Press) then exit;
      if (ksAltGr in value[index].Shift) and not adjustShift(ksAltGr, value[index].Press) then exit;
      inc(index);
    end;
  finally
    if aResult = 0 then
      for i := 0 to 255 do
        if KeyDown[i] then begin
          aResult := -6;
          break;
        end;
    if (aResult = 0) and (shiftstate <> []) then
      aResult := -7;
    result := aResult;
  end;
end;


{$ifdef DEBUG}
initialization
  if sizeof(TKbdShift) > sizeof(byte) then
    Raise Exception.Create('Size of TkbdShift > size of byte, adjust TKbdEvent.EventToHex and TKbdEvent.HexToEvent');
{$ENDIF}
end.

