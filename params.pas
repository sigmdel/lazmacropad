unit params;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

CONST
  DEFAULT_ROW_COUNT = 3;
  DEFAULT_COL_COUNT = 6;
  OPTIONS_FILENAME = 'options.ini';
  DEFAULT_MACROS = 'default.macros';

type
  TLogLevel = (llDebug, llInfo, llError, llNone);
  TPasteCommand = (pcCtrlV, pcShiftInsert, pcNone);

var
  Macros: array of string;
  Pastes: array of TPasteCommand;
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

implementation

uses
  inifiles, macrolog;

var
  configfile: string;


procedure SaveMacros(const filename: string);
var
  i,n: integer;
begin
  n := 0;
  with TInifile.create(filename) do try
    for i := 0 to config.ButtonCount-1 do begin
       WriteString('macros', inttohex(i,2), macros[i]);
       if macros[i] <> '' then inc(n);
    end;
    for i := 0 to config.ButtonCount-1 do begin
       WriteInteger('pastes', inttohex(i,2), ord(pastes[i]));
    end;
     MacrosModified := false;
     LogForm.log(llInfo,'Macros file %s with %d macro definitions saved', [filename, n]);
  finally
    free;
  end;
end;

procedure LoadMacros(const filename: string);
var
  i, n: integer;
  s: string;
begin
  MacrosModified := false;
  for i := 0 to config.ButtonCount-1 do begin
     macros[i] := '';
     pastes[i] := pcCtrlV;
  end;

  n := 0;
  if (filename = '') then
    LogForm.log(llError, 'params.LoadMacros() called with empty filename')
  else if not fileexists(filename) then
    LogForm.log(llError, 'Macros file cannot be loaded, %s does not exist', [filename])
  else with TInifile.create(filename) do try
     for i := 0 to config.ButtonCount-1 do begin
        s := ReadString('macros', inttohex(i,2), macros[i]);
        if s <> '' then begin
          inc(n);
          macros[i] := s;
        end;
     end;
     for i := 0 to config.ButtonCount-1 do begin
        pastes[i] := TPasteCommand(ReadInteger('pastes', inttohex(i,2), ord(pastes[i])));
     end;
     LogForm.log(llInfo,'Macros file %s contained %d macro definitions', [filename, n]);
  finally
    free;
  end;
end;

{ TConfig }

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
  OnGetVendorName := @Vendor;
  OnGetApplicationName := @GetAppName;
  configdir := GetAppConfigDir(false);
  ForceDirectories(configdir);   // create config directory, report error if false ?
  configdir := IncludeTrailingPathDelimiter(configdir);
  configfile := configdir + OPTIONS_FILENAME;
  config := TConfig.create;
  config.Load;
  if Config.DefaultMacrosfile = '' then
     Config.DefaultMacrosFile := configdir + DEFAULT_MACROS;
  setlength(Macros, config.ButtonCount);
  setlength(Pastes, config.ButtonCount);
  LoadMacros(Config.DefaultMacrosfile);
end;

finalization
  config.free;
  setlength(Macros, 0);
  setlength(Pastes, 0);
end.

