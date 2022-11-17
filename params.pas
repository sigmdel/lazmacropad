unit params;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, kbdev;

CONST
  DEFAULT_ROW_COUNT = 4;
  DEFAULT_COL_COUNT = 4;
  OPTIONS_FILENAME = 'options.ini';
  DEFAULT_MACROS = 'default.macros';

type
  TLogLevel = (llDebug, llInfo, llError, llNone);
  TPasteCommand = (pcCtrlV, pcShiftInsert, pcCustom, pcNone, pcKbdEvents);

var
  // list of current paste commands
  Pastes: array of TPasteCommand;

  // List of current macros
  StringMacros: array of string; // all macros no matter the type stored here
  KbdMacros: array of TKbdMacro; // only TKbdmacros are store here
  MacrosModified: boolean;

procedure SaveMacros(const filename: string);
procedure LoadMacros(const filename: string);

type
  TConfig = class
  private
    FDeviceName: string;
    FBaud: longint;
    FKeyCols: integer;
    FKeyRows: integer;
    FLoglevel: TLogLevel;
    FLogSize: integer;
    FDefaultMacrosFile: string;
    FModified: boolean;
    FButtonCount: integer;
    procedure SetBaud(AValue: longint);
    procedure SetDefaultMacrosFile(const AValue: string);
    procedure SetDeviceName(AValue: string);
    procedure SetLogLevel(AValue: TLogLevel);
  public
    procedure ClearMacros;
    constructor Create;
    procedure Save;
    procedure Load;
    property Baud: longint read FBaud write SetBaud;
    property ButtonCount: integer read FButtonCount;
    property DefaultMacrosFile: string read FDefaultMacrosFile write SetDefaultMacrosFile;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property KeyRows: integer read FKeyRows;
    property KeyCols: integer read FKeyCols;
    property Modified: boolean read FModified write FModified;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
    property LogSize: integer read FLogSize;
  end;

var
  Config: TConfig;
  ConfigDir: string;

procedure ParamsInit;

const
 SLogLevel: array[TLogLevel] of string = ('Debug', 'Info', 'Error', 'None');


implementation

uses
  inifiles, macrolog;

var
  configfile: string;

(* Keyboard macros are saved as concatened hex kbd events
 *
 * Syntax of hex kbd event
 *
 *  |--|<-----------------TKbdEvent.Press
 *  |--| |--|<----------- TKbdEvent.VK
 *  |--| |--|  |----|<--- TKbdEvent.Shift
 *   01   7B    0001
 *
 * There are no spaces so each event takes is 6 hexadecimal
 * characters wide.
 *)
procedure SaveMacros(const filename: string);
var
  i,smcount, kmcount: integer;
begin
  smcount := 0;
  kmcount := 0;
  with TInifile.create(filename) do try
    for i := 0 to config.ButtonCount-1 do begin
       if pastes[i] = pcKbdEvents then begin
         WriteString('macros', inttohex(i,2), KbdMacroToHex(KbdMacros[i]));
         if length(KbdMacros[i]) > 0 then inc(kmcount);
       end
       else begin
         WriteString('macros', inttohex(i,2), StringMacros[i]);
         if StringMacros[i] <> '' then inc(smcount);
       end;
    end;
    for i := 0 to config.ButtonCount-1 do begin
       WriteInteger('pastes', inttohex(i,2), ord(pastes[i]));
    end;
     MacrosModified := false;
     LogForm.log(llInfo,'Macros file %s with %d string macros and %d keyboard macros saved', [filename, smcount, kmcount]);
  finally
    free;
  end;
end;

{ #todo 1 -oMichel -cBug : No real error checking  }
procedure LoadMacros(const filename: string);
var
  i, smcount, kmcount: integer;
  s: string;
begin
  Config.ClearMacros;
  MacrosModified := false;
  smcount := 0;
  kmcount := 0;
  if (filename = '') then
    LogForm.log(llError, 'params.LoadMacros() called with empty filename')
  else if not fileexists(filename) then
    LogForm.log(llError, 'Macros file cannot be loaded, %s does not exist', [filename])
  else with TInifile.create(filename) do try
    for i := 0 to config.ButtonCount-1 do begin
       pastes[i] := TPasteCommand(ReadInteger('pastes', inttohex(i,2), ord(pastes[i])));
    end;
    for i := 0 to config.ButtonCount-1 do begin
      s := ReadString('macros', inttohex(i,2), StringMacros[i]);
      if s <> '' then begin
        if pastes[i] = pcKbdEvents then begin
          KbdMacros[i] := HexToKbdMacro(s);
          StringMacros[i] := KbdMacroToStr(KbdMacros[i]);
          inc(kmcount);
        end
        else begin
          StringMacros[i] := s;
          inc(smcount);
        end;
      end;
    end;
    LogForm.log(llInfo,'Macros file %s contained %d string macros and %d keyboard macros', [filename, smcount, kmcount]);
  finally
    free;
  end;
end;

{ TConfig }

procedure TConfig.ClearMacros;
var
  i: integer;
begin
  for i := 0 to config.ButtonCount-1 do begin
     StringMacros[i] := '';
     setlength(KbdMacros[i], 0);
     pastes[i] := pcCtrlV;
  end;
end;

constructor TConfig.Create;
begin
  {$ifdef LINUX}
  FDeviceName := '/dev/ttyUSB0';
  {$endif}
  {$ifdef WINDOWS}
  FDeviceName := 'COM4';
  {$endif}
  FBaud := 9600;
  FKeyCols := DEFAULT_COL_COUNT;
  FKeyRows := DEFAULT_ROW_COUNT;
  FButtonCount := FKeyCols*FKeyRows;
  FLogLevel := llInfo;
  FLogSize := 256;
  FDefaultMacrosFile := '';
end;

procedure TConfig.Load;
begin
  if not fileexists(configfile) then begin
    // make sure a config file always exists
    Save;
    LogForm.log(llInfo, 'Default configuration file created because none was found');
    exit;
  end;
  with TInifile.create(configfile) do try
    FDeviceName := ReadString('serial', 'device', FDeviceName);
    FBaud := ReadInteger('serial', 'baud', FBaud);
    FkeyCols := ReadInteger('keypad', 'cols', FkeyCols);
    FKeyRows := ReadInteger('keypad', 'rows', FkeyRows);
    FButtonCount := FKeyCols*FKeyRows;
    if FButtonCount < 1 then begin
      FKeyCols := DEFAULT_COL_COUNT;
      FKeyRows := DEFAULT_ROw_COUNT;
      FButtonCount := FKeyCols*FKeyRows;
    end;
    Floglevel := TLogLevel(ReadInteger('log', 'level', ord(Floglevel)));
    FLogSize := ReadInteger('log', 'size', FLogSize);
    FDefaultMacrosFile := ReadString('macros', 'filename', FDefaultMacrosFile);
    if not fileexists(FDefaultMacrosfile) and fileexists(configdir+FDefaultMacrosfile) then
       FDefaultMacrosfile := configdir+FDefaultMacrosfile;
    FModified := false;
  finally
    free;
  end;
  LogForm.log(llInfo, 'Configuration file %s loaded', [configfile]);
end;

procedure TConfig.Save;
begin
  with TInifile.create(configfile) do try
     WriteString('serial', 'device', FDeviceName);
     WriteInteger('serial', 'baud', FBaud);
     WriteInteger('keypad', 'cols', FkeyCols);
     WriteInteger('keypad', 'rows', FkeyRows);
     WriteInteger('log', 'level', ord(FLoglevel));
     WriteInteger('log', 'size', FLogSize);
     writeString('macros', 'filename', FDefaultMacrosFile);
     FModified := false;
   finally
     free;
   end;
   LogForm.log(llInfo, 'Configuration file %s saved', [configfile]);
end;

procedure TConfig.SetBaud(AValue: longint);
begin
  if FBaud=AValue then Exit;
  FBaud:=AValue;
  FModified := true;
end;

procedure TConfig.SetDefaultMacrosFile(const AValue: string);
begin
  if FDefaultMacrosFile=AValue then Exit;
  FDefaultMacrosFile:=AValue;
  with TInifile.create(configfile) do try
    writeString('macros', 'filename', FDefaultMacrosFile);
  finally
    free;
  end;
end;

procedure TConfig.SetDeviceName(AValue: string);
begin
  if FDeviceName=AValue then Exit;
  FDeviceName:=AValue;
  FModified := true;
end;

procedure TConfig.SetLogLevel(AValue: TLogLevel);
begin
  if FLogLevel=AValue then Exit;
  FLogLevel:=AValue;
  FModified := true;
end;

{ initialization }

function Vendor: string;
begin
  result := 'sigmdel';
end;

function GetAppName: string;
begin
  result := changefileext(extractfilename(paramstr(0)), '');
end;

// Can't do this in initialization because LogForm.log is used
// in config.Load which thus requires LogForm to be created
procedure ParamsInit;
begin
  config.Load;
  if Config.DefaultMacrosfile = '' then
     Config.DefaultMacrosFile := configdir + DEFAULT_MACROS;
  setlength(StringMacros, config.ButtonCount);
  setlength(KbdMacros, config.ButtonCount);
  setlength(Pastes, config.ButtonCount);
  LoadMacros(Config.DefaultMacrosfile);
end;

initialization
  OnGetVendorName := @Vendor;
  OnGetApplicationName := @GetAppName;
  configdir := GetAppConfigDir(false);
  ForceDirectories(configdir);   // create config directory, report error if false ?
  configdir := IncludeTrailingPathDelimiter(configdir);
  configfile := configdir + OPTIONS_FILENAME;
  config := TConfig.create;

finalization
  config.free;
  (*
  setlength(StringMacros, 0);
  setlength(KbdMacros, 0); // this will strand allocated memory
  setlength(Pastes, 0);
  *)
end.

