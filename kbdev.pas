unit kbdev;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

type
  TKbdShiftEnum = ssShift..ssCtrl;
  TKbdShift = set of TKbdShiftEnum;

  TKbdEvent = record
    Press: boolean;
    VK: word;
    Shift: TKbdShift;
  end;
  KbdEvent = ^TKbdEvent;

  TKbdMacro = array of TKbdEvent;

// returns key name of value.VK or '<?xxx?>' if the VK is unknown
function KbdEventToKeyStr(value: TKbdEvent): string;

// returns key name and shift state as in 'F23[Shift,Alt]'
function KbdEventToStr(value: TKbdEvent): string;

// returns a string representation of a kbd event macro
function KbdMacroToStr(value: TKbdMacro): string;

// converts a string to a kbd event macro
function StrToMacro(const value: string): TKbdMacro;

// incomplete test of validity
function TestMacro(value: TKbdMacro): integer;

const
  // MS documentation:
  // https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  // keys ordered by code, so a binary search can be used on the code
  KEYCOUNT = 132;
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
    (name: '0'; code: $30),
    (name: '1'; code: $31),
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
    (name: 'X'; code: $58),
    (name: 'Y'; code: $59),
    (name: 'Z'; code: $5A),
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
    (name: 'OEM_0x92'; code: $92), // 0x92
    (name: 'OEM_0x93'; code: $93), // 0x93
    (name: 'OEM_0x94'; code: $94), // 0x94
    (name: 'OEM_0x95'; code: $95), // 0x95
    (name: 'OEM_0x96'; code: $96), // 0x96
    (name: ';'; code: $ba), //- VK_OEM_1 - Can vary by keyboard, US keyboard, the ';:' key
    (name: '='; code: $bb), // VK_OEM_PLUS - For any country/region, the '+/=' key Delphi returns '=' Issue #0036489
    (name: ','; code: $bc), // VK_OEM_COMMA - For any country/region, the ',' key
    (name: '-'; code: $bd), // VK_OEM_MINUS - For any country/region, the '-' key
    (name: '.'; code: $be), // VK_OEM_PERIOD - For any country/region, the '.' key
    (name: '/'; code: $bf), // VK_OEM_2 - Can vary by keyboard, US keyboard, the '/?' key
    (name: '`'; code: $c0), // VK_OEM_3 - Can vary by keyboard, US keyboard, the '`~' key
    (name: '['; code: $db), // VK_OEM_4 - Can vary by keyboard, US keyboard, the '[{' key
    (name: '\'; code: $dc), // VK_OEM_5 - Can vary by keyboard, US keyboard, the '\|' key
    (name: ']'; code: $dd) // VK_OEM_6 - Can vary by keyboard, US keyboard, the ']}' key
  );
  { #todo -oMichel -cRefactoring : Use Classes TIdentMapEntry, IntToIdent etc. instead }

function KeyCodeFind(key: integer; var index: integer): boolean;
function KeyNameIndex(const name: string; var index: integer): boolean;

const
  DefaultEvent: TKbdEvent = (Press: true; VK: VK_F13; Shift: []);

implementation

uses
  LCLProc, StrUtils, LCLStrConsts;



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

// returns key name of value.VK or '<?xxx?>' if the VK is unknown
function KbdEventToKeyStr(value: TKbdEvent): string;
var
  index: integer;
begin
  if KeyCodeFind(value.VK, index) then
    result := KeyCodesAndStrings[index].Name
  else
    result := '<$'+IntToHex(value.VK,2)+'?>';
end;

// returns key name and shift state as in 'F23[Shift,Alt]'
function KbdEventToStr(value: TKbdEvent): string;
var
  s: string;

  procedure AddPart(const APart: string);
  begin
    if s <> '' then
      s := s + ',';
    s := s + APart;
  end;

begin
  Result := '';
  s := '';
  if ssCtrl in value.Shift then AddPart(ifsCtrl);
  if ssAlt in value.Shift then AddPart(ifsAlt);
  if ssShift in value.Shift then AddPart(ifsVK_SHIFT);
  (*
  if ssMeta in ShiftState then
    {$IFDEF LCLcarbon}
    AddPart(ifsVK_CMD);
    {$ELSE}
    AddPart(ifsVK_META);
    {$ENDIF}
  if ssSuper in ShiftState then AddPart(ifsVK_SUPER);
  *)
  s := '[' + s + ']';
  result := KbdEventToKeyStr(value) + s;
  result := ifThen(value.Press, '↓', '↑') + ' ' + result;
end;

// returns a string representation of a kbd event macro
function KbdMacroToStr(value: TKbdMacro): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to length(value)-1 do begin
    if result <> '' then
      result := result + ' ';
    result := result + '(' + KbdEventToStr(value[i]) + ')';
  end;
end;

function MakeTKbEvent(press: boolean; key: word; shift: TKbdShift): KbdEvent;
begin
  new(result);
  result^.press := press;
  result^.VK := key;
  result^.shift := shift;
end;

function TestMacro(value: TKbdMacro): integer;
var
  aResult: integer;
  i: integer;
  count: integer;
  delta: integer;
  temp: integer;

function adjustCount: boolean;
begin
  count := count + delta;
  if count >= 0 then
    result := true
  else begin
    aResult := -3;
    result := false;
  end;
end;

begin
  aResult := 0;
  count := 0;
  try
    if length(value) < 1 then begin
      aResult := -1;
      exit;
    end;
    for i := 0 to length(value)-1 do begin
      if value[i].Press then
        delta := 1
      else
        delta := -1;
      //if KeyCodeToKeyString(value[i].VK, true) = '' then begin
      if not KeyCodeFind(value[i].VK, temp) then begin
        aResult := -2;
        exit;
      end;
      if not adjustCount then exit;
      if (ssShift in value[i].Shift) and not adjustCount then exit;
      if (ssCtrl in value[i].Shift) and not adjustCount then exit;
      if (ssAlt in value[i].Shift) and not adjustCount then exit;
    end;
    if count <> 0 then
      aResult := -4;
  finally
    result := aResult;
  end;
end;

function StrToMacro(const value: string): TKbdMacro;
var
  s: string;
  kname, shifts: string;
  p, i, ml: integer;
  anEvent: TKbdEvent;
begin
  setlength(result, 0);
  ml := 0;
  s := StringReplace(value, ' ', '', [rfReplaceAll]);
  s := StringReplace(s, '(', '', [rfReplaceAll]);
  s := StringReplace(s, ')', '', [rfReplaceAll]);
  while length(s) > 0 do begin
    anEvent.Press := pos('↓', s) = 1;
    delete(s,1,length('↓'));

    kname := s[1];
    delete(s, 1, 1);
    while s[1] <> '[' do begin
      kname := kname + s[1];
      delete(s, 1, 1);
    end;
    delete(s,1,1);
    shifts := '';
    p := pos(']', s);
    if p > 0 then begin
      shifts := copy(s, 1, p-1);
      delete(s, 1, p);
    end;
    if KeyNameIndex(kname, i) then begin
      anEvent.VK := KeyCodesAndStrings[i].code;
      anEvent.Shift := [];
      if pos('Shift', shifts) > 0 then include(anEvent.Shift, ssShift);
      if pos('Ctrl', shifts) > 0 then include(anEvent.Shift, ssCtrl);
      if pos('Alt', shifts) > 0 then include(anEvent.Shift, ssAlt);
      setlength(result, ml+1);
      result[ml] := anEvent;
      inc(ml);
    end;
  end;
end;

end.

