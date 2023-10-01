unit umacros;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ukbdev, ulog;

const
  DEFAULT_VK_RETURN_DELAY = 50;  // 30 ms seems adequate, the safety margin is not noticeable

type
  { TMacro }

     // There are two types of macros:
     // TKbdMacro defined in ukbdev and
  TStrMacro = string;

     // There are five kinds of macros, the first 4 kinds are TStrMacro types
     // the last TKbdMacro type.
  TMacroKind = (pcCtrlV, pcShiftInsert, pcCustom, mkNone, mkKbdMacro);

     // The first 3 kinds of TStrMacro are pasted into the system clipboard
     // (and selection in Linux) and then a keyboard shortcut is used to paste
     // the clipboard (or selection) into the currently running application
  TPasteShortcut = pcCtrlV..pcCustom;

    { TMacroRec }

  TMacroRec = record
    Content: TStrMacro;
    Events: TKbdMacro;
    Kind: TMacroKind;
  end;

  TMacroList = class; // forward declaration

    // Each macro, no matter the type is stored in a TMacro class
    // and all TMacros are stored in a TMacroList
  TMacro = class
  private
    FList: TMacroList; // needed to call the list's SetModified function
    Findex: integer;   // only used in logging messages
    FMacro: TMacroRec; // the actual macro data
    procedure setContent(AValue: TStrMacro);
    procedure setEvents(AValue: TKbdMacro);
    procedure setKind(AValue: TMacroKind);
    procedure setMacro(AValue: TMacroRec);
    procedure setModified;
    procedure DoClear;
  public
      // Sets Content to '', Events to nil, and Pastecmd to pcCtrlV
    procedure Clear;

      // Returns true if content = '', events = nil, and pastecmd = pcCtrlV
    function isEmpty: boolean;

      // Returns true if value is the same as this macro
    function isEqual(const value: TMacro): boolean;

      // Macro is a string macro if pastecmd <> mkKbdMacro or the
      // string representation of events if pastecmd = mkKbdMacro
    property content: TStrMacro read FMacro.Content write setContent;

      // Macro is a kbd macro if pastecmd = mkKbdMacro
      // or nil if pastecmd <> mkKbdMacro
    property events: TKbdMacro read FMacro.Events write setEvents;

      // Macro data (content, events and pastecmd) only
    property macro: TMacroRec read FMacro; // write setMacro;

      // Read only property giving the index number of the macro in
      // the macros array of the owning TMacroList.
    property index: integer read findex;

      // Read only property giving the TMacroList that owns the macro.
    property List: TMacroList read FList;

      // Defines the kind of macro. Changing from a kind < pcKbdEvent to
      // pcKbdEvent (or vice-versa) has side effects on Content and Events.
    property Kind: TMacroKind read FMacro.Kind write setKind;
  end;

  TMacroArray = array of TMacro;

  { TMacroList }

  TMacroList = class
  private
    FItems: TMacroArray;
    FSize: integer;
    FPasteShortcut: array[TPasteShortcut] of TKbdEvent;
    FReturnDelay: integer;
    FFilename: string;
    FModified: boolean;
    FOnModified: TNotifyEvent;
    function Get(index: integer): TMacro;
    function GetCustomShortcut: TKbdEvent;
    function GetPasteDelay(index: TPasteShortcut): integer;
    procedure Put(index: integer; AValue: TMacro);
    procedure SetCustomShortcut(AValue: TKbdEvent);
    procedure SetModified(value: boolean);
    function IndexOutofRange(const index: integer): boolean;
    function GetPasteShortcut(index: TPasteShortcut): TKbdEvent;
    procedure InitShortcuts;
    procedure SetPasteDelay(index: TPasteShortcut; AValue: integer);
    procedure SetReturnDelay(AValue: integer);
  protected
    procedure SetSize(const AValue: integer);
  public
    constructor create;

    destructor Destroy; override;

      // Clears each macro in the list
    procedure Clear;

      // Saves macros to an ini file
    procedure Save(const filename: string);

      // Loads macros from an ini file
    procedure Load(const filename: string);

      // Exchanges two macros as given positions
    procedure Exchange(const index1, index2: integer);

      // Clear the macro at index. Size remains unchanged
    procedure Erase(const index: integer);

      // Deletes a single macro, moving up following macros. Size remains unchanged
    procedure Delete(const index: integer);

      // Inserts an empty macro at the given index. Size remains unchanged so
      // the last macro in the list is lost.
    procedure Insert(const index: integer); overload;

      // Inserts a given macro at the given index. Size remains unchanged so
      // the last macro in the list is lost.
    //procedure Insert(const index: integer; value: TMacro); overload;

    property Filename: string read FFilename;

      // The array of TMacros only
    property Items: TMacroArray read FItems;

      // Individual macros
    property Macro[index: integer]: TMacro read Get write Put; Default;

      // Set to true when a macro is modified, deleted or moved
    property Modified: boolean read FModified write SetModified;

      // Microsecond delay before the corresponding paste command is injected
      // Corresponds to PasteShortcut[index].DelayMS, with the addition that it is
      // writable.
    property PasteDelay[index: TPasteShortcut]: integer read GetPasteDelay write SetPasteDelay;

      // Three clipboard paste commands (for pcCtrlV..pcCustom); read only.
    property PasteShortcut[index: TPasteShortcut]: TKbdEvent read GetPasteShortcut;

    property CustomShortcut: TKbdEvent read GetCustomShortcut write SetCustomShortcut;

      // Number of macros in the list
    property Size: integer read FSize write SetSize;

      // Millisecond delay before a VK_RETURN is output in lieu of a '\n'
      // sequence at the end of a str macro
    property ReturnDelay: integer read FReturnDelay write SetReturnDelay;

      // Called when FOnModified is changed
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

const
  sMacroKind: array[TMacroKind] of string = (
    'Ctrl+V',
    'Shift+Insert',
    'Custom',
    'None',
    'KbdMacro');

var
  macros: TMacroList;   // list of macros

operator = (const mac1, mac2: TMacroRec): boolean;

implementation

uses
  LCLType, inifiles;

function ellipsis(const source: string; len: integer): string;
begin
  result := source;
  if length(result) > len then begin
    setlength(result, len);
    result := result + ' ...';
  end;
end;

{ TMacro }

operator = (const mac1, mac2: TMacroRec): boolean;
begin
  result := (mac1.Content = mac2.Content) and
    (mac1.Events = mac2.Events) and (mac1.Kind = mac2.Kind);
end;

procedure TMacro.setContent(AValue: TStrMacro);
const
  _log_name_ = 'TMacro.setContent';
begin
  if Content = AValue then Exit;
  FMacro.Content := AValue;
  setlength(FMacro.Events, 0);
  if Kind = mkKbdMacro then
    FMacro.Kind := pcCtrlV;
  LogOut(llDebug, _log_name_+': '+'Str macro %d set to "%s"', [index, ellipsis(content, 48)]);
  setModified;
end;

procedure TMacro.setEvents(AValue: TKbdMacro);
const
  _log_name_ = 'TMacro.setEvents';
begin
  if FMacro.Events = AValue then Exit;

  FMacro.Events := AValue;
  FMacro.Content := KbdMacroToStr(Events);
  FMacro.Kind := mkKbdMacro;
  LogOut(llDebug, _log_name_+': '+'Kbd macro %d set to "%s"', [index, ellipsis(content, 48)]);
  setModified;
end;

procedure TMacro.setMacro(AValue: TMacroRec);
begin
  FMacro := AValue;
end;

procedure TMacro.setModified;
begin
  if assigned(FList) then
    FList.Modified := true;
end;

procedure TMacro.setKind(AValue: TMacroKind);
const
  _log_name_ = 'TMacro.setPastecmd';
begin
  if Kind = AValue then Exit;
  FMacro.Kind := AValue;
  if Kind = mkKbdMacro then
    FMacro.Content := KbdMacroToStr(Events)
  else
    setlength(FMacro.Events, 0);
  LogOut(llDebug, _log_name_+': '+'Paste command of macro %d set to %s', [index, sMacroKind[AValue]]);
  setModified;
end;

procedure TMacro.DoClear;
begin
  FMacro.Content := '';
  setlength(FMacro.Events, 0);
  FMacro.Kind := pcCtrlV;
end;

procedure TMacro.Clear;
const
  _log_name_ = 'TMacro.Clear';
begin
  if isEmpty then exit;
  DoClear;
  LogOut(llDebug,_log_name_+': '+'Macro %d cleared', [index]);
  setModified;
end;

function TMacro.isEmpty: boolean;
begin
  result := (Content = '') and (length(Events) = 0) and (Kind = pcCtrlV);
end;

function TMacro.isEqual(const value: TMacro): boolean;
begin
  result := Kind = value.Kind;
  if not result then exit;
  if Kind = mkKbdMacro then
    result := events = value.events
  else
    result := content = value.content
end;

{ TMacroList }

constructor TMacroList.create;
begin
  inherited Create;
  InitShortCuts;
  FReturnDelay := DEFAULT_VK_RETURN_DELAY;
end;

destructor TMacroList.Destroy;
var
  i: integer;
begin
  for i := 0 to length(FItems)-1 do
    freeandnil(FItems[i]);
  inherited Destroy;
end;

procedure TMacroList.Clear;
const
  _log_name_ = 'TMacroList.Clear';
var
  i: integer;
  changed: boolean;
begin
  changed := false;
  FFilename := '';
  for i := 0 to length(FItems)-1 do begin
    FItems[i].FList := self;
    FItems[i].FIndex := i;
    if FItems[i].IsEmpty then continue;
    FItems[i].DoClear;
    changed := true;
  end;
  if changed then begin
    Modified := true;
    LogOut(llDebug, _log_name_+': '+'Cleared');
  end;
end;

procedure TMacroList.Delete(const index: integer);
const
  _log_name_ = 'TMacroList.Delete';
var
  i: integer;
  change: boolean;
  tmp: TMacro;
begin
  if IndexOutofRange(index) then exit;
  change := false;
  for i := index to length(FItems)-1 do
    if not FItems[i].isEmpty then
      change := true;
  if not change then begin
    LogOut(llDebug, _log_name_+': '+'All macros from index %d on already empty', [index]);
    exit;
  end;

  tmp := FItems[index];  // reuse the deleted TMacro
  for i := index to length(FItems)-2 do
    FItems[i] := FItems[i+1];
  tmp.Clear;
  FItems[length(FItems)-1] := tmp;
  for i := index to length(FItems)-1 do
    FItems[i].Findex := i;
  Modified := true;
  LogOut(llDebug, _log_name_+': '+'Macro at index %d deleted', [index]);
end;

procedure TMacroList.Erase(const index: integer);
const
  _log_name_ = 'TMacroList.Exchange';
begin
  if IndexOutofRange(index) then exit;
  FItems[index].Clear;
  LogOut(llDebug, _log_name_+': '+'Macros %d erased', [index]);
end;

procedure TMacroList.Exchange(const index1, index2: integer);
const
  _log_name_ = 'TMacroList.Exchange';
var
  tmp: TMacro;
begin
  if (index1 = index2) or IndexOutofRange(index1) or indexOutOfRange(index2)
    then exit;
  tmp := FItems[index1];
  FItems[index1] := FItems[index2];
  FItems[index2] := tmp;
  FItems[index1].Findex := index1;
  FItems[index2].Findex := index2;
  Modified := true;
  LogOut(llDebug, _log_name_+': '+'Macros at indices %d and %d exchanged', [index1, index2]);
end;

function TMacroList.Get(index: integer): TMacro;
begin
  result := FItems[index];
end;

function TMacroList.GetCustomShortcut: TKbdEvent;
begin
  result := FPasteShortcut[pcCustom];
end;

function TMacroList.GetPasteDelay(index: TPasteShortcut): integer;
begin
  result := FPasteShortcut[index].Delayms;
end;

function TMacroList.GetPasteShortcut(index: TPasteShortcut): TKbdEvent;
begin
  result := FPasteShortcut[index];
end;

function TMacroList.IndexOutofRange(const index: integer): boolean;
begin
  if (index < 0) or (index >= length(FItems)) then begin
    logOut(llError, 'Index (%d) is out of range', [index]);
    result := true;
    exit;
  end;
  result := false;
end;

procedure TMacroList.InitShortcuts;
begin
  with FPasteShortcut[pcCtrlV] do begin
    Code := VK_V;             // read only
    Shift := [ksCtrl];        // read only
    Delayms := 0;             // read, write
    Press := true;            // ignored
  end;
  with FPasteShortcut[pcShiftInsert] do begin
    Code := VK_INSERT;         // read only
    Shift := [ksShift];        // read only
    Delayms := 0;              // read, write
    Press := true;             // ignored
  end;
  with FPasteShortcut[pcCustom] do begin
    Code := VK_V;                // read, write
    Shift := [ksShift, ksCtrl];  // read, write
    Delayms := 0;                // read, write
    Press := true;               // ignored
  end;
end;

procedure TMacroList.Insert(const index: integer);
const
  _log_name_ = 'TMacroList.Insert';
var
  i: integer;
  change: boolean;
  tmp: TMacro;
begin
  if IndexOutofRange(index) then exit;
  change := false;
  for i := index to length(FItems)-1 do
    if not FItems[i].isEmpty then
      change := true;
  if not change then begin
    LogOut(llDebug, _log_name_+': '+'All macros from index %d on already empty', [index]);
    exit;
  end;

  tmp := FItems[length(FItems)-1]; // reuse the last macro that will be deleted
  for i := length(FItems)-1 downto index+1 do
    FItems[i] := FItems[i-1];
  tmp.Clear;
  FItems[index] := tmp;
  for i := index to length(FItems)-1 do
    FItems[i].Findex := i;
  Modified := true;
  LogOut(llDebug, _log_name_+': '+'Empty macro inserted at index %d', [index]);
end;

procedure TMacroList.Load(const filename: string);
const
  _log_name_ = 'TMacroList.Load';
var
  i, n, macrocount: integer;
  s: string;
  ke: TKbdEvent;
begin
  if (filename = '') or not fileexists(filename) then begin
    LogOut(llError, _log_name_+': '+'Macros file "%s" not found', [extractfilename(filename)]);
    exit;
  end;
  with TInifile.create(filename) do try
    LogOut(llDebug, _log_name_+': '+'Loading "%s"', [extractfilename(filename)]);
    Clear;
    // First read pastcmd to know the type of macros to read
    // By default they are set to pcCtrlV even if not found.
    for i := 0 to size-1 do begin
      n := ReadInteger('pastes', inttohex(i,2), -1);
      if (n > 0) and (n <= ord(mkKbdMacro)) then
        FItems[i].Kind := TMacroKind(n);
    end;

    // Read the macros
    macrocount := 0;
    for i := 0 to size-1 do begin
      s := ReadString('macros', inttohex(i,2), '');
      if (s <> '') then begin
        inc(macrocount);
        if FItems[i].Kind = mkKbdMacro then begin
          try
            FItems[i].events := HexToKbdMacro(s);
          except
            LogOut(llError,_log_name_+': '+'Error reading kbd macro %d', [i]);
          end;
        end
        else begin
          FItems[i].content := s;
        end;
      end;
    end;

    SetPastedelay(pcCtrlV, ReadInteger('pastecmd', 'ctrl-v', PasteDelay[pcCtrlV]));
    SetPastedelay(pcCtrlV, ReadInteger('pastecmd', 'shift-insert', PasteDelay[pcShiftInsert]));

    s := ReadString('pastecmd', 'custom', '');
    i := 1;
    try
      ke.HexToEvent(s, i);
    except
      ke.code := 0;
    end;
    if (ke.Code = 0) then
      logOut(llInfo, _log_name_+': '+'Macros file %s contained invalid custom paste command, using default value', [filename])
    else
      FPasteShortcut[pcCustom] := ke;

    ReturnDelay := ReadInteger('vk-return', 'delay', ReturnDelay); //
    Modified := false;
    LogOut(llInfo, _log_name_+': '+'Read %d macros from %s', [macrocount, extractfilename(filename)]);
    FFilename := filename;
  finally
    free; // inifile
  end;
end;

procedure TMacroList.Put(index: integer; AValue: TMacro);
begin
  if FItems[index].isEqual(AValue) then exit;
  FItems[index] := AValue;
  Modified := true;
end;

procedure TMacroList.Save(const filename: string);
const
  _log_name_ = 'TMacroList.Save';
var
  i: integer;
begin
  if length(FItems) < 1 then begin
    LogOut(llError, _log_name_+': '+'Macros not initialized');
    exit;
  end;
  { #todo 2 : Backup the old inifile if it exists }
  with TInifile.create(filename) do try
    EraseSection('macros');
    EraseSection('pastes');
    EraseSection('pastecmd');
    EraseSection('vk-return');
    // write each macro's Kind
    for i := 0 to length(FItems)-1 do begin
       WriteInteger('pastes', inttohex(i,2), ord(FItems[i].Kind));
    end;
    // write each macro's content
    for i := 0 to length(FItems)-1 do begin
       if FItems[i].Kind = mkKbdMacro then
         WriteString('macros', inttohex(i,2), KbdMacroToHex(FItems[i].events))
       else
         WriteString('macros', inttohex(i,2), FItems[i].content);
    end;
    WriteInteger('pastecmd', 'ctrl-v', PasteShortcut[pcCtrlV].Delayms);
    WriteInteger('pastecmd', 'shift-insert', PasteShortcut[pcShiftInsert].Delayms);
    WriteString('pastecmd', 'custom', PasteShortcut[pcCustom].EventToHex);
    WriteInteger('vk-return', 'delay', ReturnDelay);
    Modified := false;
    LogOut(llInfo, _log_name_+': '+'Saved %d macros to %s', [length(FItems), filename]);
    FFilename := filename;
  finally
    free;  //inifile
  end;
end;

procedure TMacroList.SetCustomShortcut(AValue: TKbdEvent);
begin
  if CustomShortCut = AValue then exit;
  FPasteShortCut[pcCustom] := AValue;
  Modified := true;
end;

procedure TMacroList.SetModified(value: boolean);
begin
  if FModified = value then exit;
  FModified := value;
  if assigned(FOnModified) then
    FOnModified(self);
end;

procedure TMacroList.SetPasteDelay(index: TPasteShortcut; AValue: integer);
const
  _log_name_ = 'TMacroList.SetPasteDelay';
begin
  if AValue < 0 then begin
    LogOut(llError,  _log_name_+': '+'%s shortcut delay cannot be negative', [sMacroKind[index]]);
    exit;
  end;
  if FPasteShortcut[index].Delayms = AValue then exit;
  FPasteShortcut[index].Delayms := AValue;
  Modified := true;
  LogOut(llDebug,  _log_name_+': '+'%s shortcut delay set to %d ms', [sMacroKind[index], Avalue]);
end;

procedure TMacroList.SetReturnDelay(AValue: integer);
const
  _log_name_ = 'TMacroList.SetReturnDelay';
begin
  if Avalue < 0 then begin
    logOut(llInfo,  _log_name_+': '+'Return delay cannot be negative');
    exit;
  end;
  if FReturnDelay = AValue then Exit;
  FReturnDelay := AValue;
  Modified := true;
  logOut(llDebug,  _log_name_+': '+'Return delay set to %d ms', [AValue]);
end;

procedure TMacroList.SetSize(const AValue: integer);
const
  _log_name_ = 'TMacroList.SetSize';
var
  i, nsz: integer;
begin
  if (AValue < 0) then
    LogOut(llError, _log_name_+': '+'Invalid number of macros (%d<0)', [AValue])
  else if (AValue = Size) then
    LogOut(llDebug, _log_name_+': '+'No change in size (%d), nothing done', [AValue])
  else begin
    if length(FItems) < FSize then begin
      for i := AValue to length(FItems)-1 do
        freeandnil(FItems[i])
    end;
    setlength(FItems, AValue);
    if length(FItems) > FSize then begin
      // add new macros
      for i := FSize to length(FItems)-1 do begin
        FItems[i] := TMacro.create; // content cleared
        FItems[i].FList := self;
        FItems[i].Findex := i;
      end;
    end;
    if FSize <> AValue then  // previous size
      Modified := true;
    FSize := length(FItems); // should = AValue
    if Size <> AValue then
      LogOut(llError, _log_name_+': '+'Unable to set size to %d', [AValue])
    else
      LogOut(llDebug, _log_name_+': '+'Resized to %d', [AValue]);
  end;
end;


initialization
  macros := TMacroList.create;
  LogOut(llDebug, 'umacros initialization completed');
  //macros.Size := DEFAULT_ROW_COUNT*DEFAULT_COL_COUNT;

finalization
  macros.free;
  LogOut(llDebug, 'umacros finalization completed');
end.

