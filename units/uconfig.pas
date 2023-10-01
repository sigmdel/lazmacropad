unit uconfig;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ulog;

const
  DEFAULT_ROW_COUNT = 4;
  DEFAULT_COL_COUNT = 4;
  OPTIONS_FILENAME  = 'options.ini';
  DEFAULT_MACROS    = 'default.macros';

type

  { TConfig }

  TConfig = class
  private
    FSerialDeviceName: string;
    FBaud: longint;
    FKeyCols: integer;
    FKeyRows: integer;
    FDefaultMacrosFile: string;
    FModified: boolean;
    FButtonCount: integer;
    function GetAppLogSize: integer;
    function GetAppLogThreshold: TLogThreshold;
    function GetConsoleLogThreshold: TLogThreshold;
    function GetSystemLogThreshold: TLogThreshold;
    procedure SetAppLogThreshold(AValue: TLogThreshold);
    procedure SetAppLogSize(AValue: integer);
    procedure SetBaud(AValue: longint);
    procedure SetDefaultMacrosFile(const AValue: string);
    procedure SetSerialDeviceName(AValue: string);
    procedure SetConsoleLogThreshold(AValue: TLogThreshold);
    procedure SetSystemLogThreshold(AValue: TLogThreshold);
    procedure Init;
  public
    constructor Create;
    procedure Load;
    procedure Save;
    function SetKeyLayout(cols, rows: integer): boolean;
    property AppLogSize: integer read GetAppLogSize write SetAppLogSize;
    property AppLogThreshold: TLogThreshold read GetAppLogThreshold write SetAppLogThreshold;
    property Baud: longint read FBaud write SetBaud;
    property ButtonCount: integer read FButtonCount;
    property ConsoleLogThreshold: TLogThreshold read GetConsoleLogThreshold write SetConsoleLogThreshold;
    property DefaultMacrosFile: string read FDefaultMacrosFile write SetDefaultMacrosFile;
    property SerialDeviceName: string read FSerialDeviceName write SetSerialDeviceName;
    property KeyRows: integer read FKeyRows;
    property KeyCols: integer read FKeyCols;
    property Modified: boolean read FModified write FModified;
    property SystemLogThreshold: TLogThreshold read GetSystemLogThreshold write SetSystemLogThreshold;
  end;

resourcestring
  SKeyLabels = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

var
  KeyLabels: string = SKeyLabels;
  Config: TConfig;
  ConfigDir: string;

//procedure LoadConfig;

implementation

uses
  umacros, iniFiles;

var
  configfile: string;

{ TConfig }

constructor TConfig.Create;
begin
  {$ifdef LINUX}
  FSerialDeviceName := '/dev/ttyUSB0';
  {$endif}
  {$ifdef WINDOWS}
  FDeviceName := 'COM4';
  {$endif}
  FBaud := 9600;
  FKeyCols := DEFAULT_COL_COUNT;
  FKeyRows := DEFAULT_ROW_COUNT;
  FButtonCount := FKeyCols*FKeyRows;
  FDefaultMacrosFile := '';
end;

procedure TConfig.Init;
const
  _log_name_ = 'TConfig';
begin
  LogOut(llDebug, _log_name_+': '+'Init');
  Load;
  if DefaultMacrosfile = '' then
     DefaultMacrosFile := configdir + DEFAULT_MACROS;
  macros.size := ButtonCount;
  macros.modified := false;
  macros.Load(Config.DefaultMacrosfile);
end;

procedure TConfig.Load;
const
  _log_name_ = 'TConfig.Load';
begin

  if not fileexists(configfile) then begin
    // make sure a config file always exists
    Save;
    LogOut(llInfo, _log_name_+': '+'Default configuration file created because none was found');
    exit;
  end;
  LogOut(llDebug, _log_name_+': '+'Found "%s"', [configfile]);

  with TInifile.create(configfile) do try
    FModified := false;
    FSerialDeviceName := ReadString('serial', 'device', FSerialDeviceName);
    FBaud := ReadInteger('serial', 'baud', FBaud);
    FkeyCols := ReadInteger('keypad', 'cols', 0);
    FKeyRows := ReadInteger('keypad', 'rows', 0);
    FButtonCount := FKeyCols*FKeyRows;
    if FButtonCount < 1 then begin
      FKeyCols := DEFAULT_COL_COUNT;
      FKeyRows := DEFAULT_ROw_COUNT;
      FButtonCount := FKeyCols*FKeyRows;
      FModified := true;
    end;
    macros.size := ButtonCount;
    ulog.AppLogThreshold := TLogThreshold(ReadInteger('log', 'applevel', ord(GetAppLogThreshold)));
    ulog.AppLogSize := ReadInteger('log', 'appsize', GetAppLogSize);
    ulog.ConsoleLogThreshold := TLogThreshold(ReadInteger('log', 'consolelevel', ord(GetConsoleLogThreshold)));
    ulog.SystemLogThreshold := TLogThreshold(ReadInteger('log', 'systemlevel', ord(GetSystemLogThreshold)));
    FDefaultMacrosFile := ReadString('macros', 'filename', FDefaultMacrosFile);
    if not fileexists(FDefaultMacrosfile) and fileexists(configdir+FDefaultMacrosfile) then begin
       FDefaultMacrosfile := configdir+FDefaultMacrosfile;
       FModified := true;
    end;
  finally
    free;
  end;
  if FModified then begin
     Save;
     LogOut(llDebug, _log_name_+': '+'Configuration file saved with corrected values');
  end
  else
    LogOut(llInfo, _log_name_+': '+'Configuration file "%s" loaded', [extractfilename(configfile)]);
end;

procedure TConfig.Save;
const
  _log_name_ = 'TConfig.Save';
begin
  with TInifile.create(configfile) do try
     WriteString('serial', 'device', FSerialDeviceName);
     WriteInteger('serial', 'baud', FBaud);
     WriteInteger('keypad', 'cols', FkeyCols);
     WriteInteger('keypad', 'rows', FkeyRows);
     WriteInteger('log', 'applevel', ord(AppLogThreshold));
     WriteInteger('log', 'appsize', AppLogSize);
     WriteInteger('log', 'consolelevel', ord(ConsoleLogThreshold));
     WriteInteger('log', 'systemlevel', ord(SystemLogThreshold));
     writeString('macros', 'filename', FDefaultMacrosFile);
     FModified := false;
   finally
     free;
   end;
   logOut(llInfo, _log_name_+': '+'Configuration file "%s" saved', [extractfilename(configfile)]);
end;

function TConfig.GetAppLogSize: integer;
begin
  result := ulog.appLogSize;
end;

function TConfig.GetAppLogThreshold: TLogThreshold;
begin
  result := ulog.appLogThreshold;
end;

function TConfig.GetConsoleLogThreshold: TLogThreshold;
begin
  result := ulog.consoleLogThreshold;
end;

function TConfig.GetSystemLogThreshold: TLogThreshold;
begin
  result := ulog.systemLogThreshold;
end;

procedure TConfig.SetAppLogThreshold(AValue: TLogThreshold);
const
  _log_name_ = 'TConfig.SetAppLogThreshold';
begin
  if AppLogThreshold = AValue then Exit;
  ulog.appLogThreshold := AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to %s', [sLogThreshold[appLogThreshold]]);
end;

procedure TConfig.SetAppLogSize(AValue: integer);
const
  _log_name_ = 'TConfig.SetAppLogSize';
begin
  if aValue < 32 then
     aValue := 32
  else if aValue > 32000 then
     aValue := 32000;
  if ulog.appLogSize = AValue then Exit;
  ulog.AppLogSize := AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to %d', [Avalue]);
end;

procedure TConfig.SetBaud(AValue: longint);
const
  _log_name_ = 'TConfig.SetBaud';
begin
  if FBaud=AValue then Exit;
  FBaud:=AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to %d', [Baud]);
end;

procedure TConfig.SetConsoleLogThreshold(AValue: TLogThreshold);
const
  _log_name_ = 'TConfig.SetConsoleLogThreshold';
begin
  if ConsoleLogThreshold = AValue then Exit;
  ulog.consoleLogThreshold := AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to %s', [sLogThreshold[consoleLogThreshold]]);
end;

procedure TConfig.SetDefaultMacrosFile(const AValue: string);
const
  _log_name_ = 'TConfig.SetDefaultMacrosFile';
begin
  if FDefaultMacrosFile=AValue then Exit;
  FDefaultMacrosFile:=AValue;
  with TInifile.create(configfile) do try
    writeString('macros', 'filename', FDefaultMacrosFile);
  finally
    free;
  end;
  LogOut(llDebug, _log_name_+': '+'Set to "%s" and config file updated', [AValue]);
end;

procedure TConfig.SetSerialDeviceName(AValue: string);
const
  _log_name_ = 'TConfig.SetSerialDeviceName';
begin
  if FSerialDeviceName=AValue then Exit;
  FSerialDeviceName:=AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to "%s"', [SerialDeviceName]);
end;

function TConfig.SetKeyLayout(cols, rows: integer): boolean;
const
  _log_name_ = 'TConfig.SetKeyLayout';
begin
  result := false;
  if (cols < 1) or (rows < 1) then
    LogOut(llError, _log_name_+': '+'Number of rows or column < 1')
  else if (cols * rows > length(KeyLabels)) then
    LogOut(llError, _log_name_+': '+'Number of keys (%d) is greater than maximum allowed (%d)', [cols*rows, length(KeyLabels)])
  else begin
    result := true;
    if (FKeyCols = cols) and (FKeyRows = rows) then
       exit; // nothing to do
    FKeyCols := cols;
    FKeyRows := rows;
    if FButtonCount <> cols*rows then begin
      FButtonCount := cols*rows;
      macros.size := FButtonCount;
      FModified := true;
    end;
    LogOut(llDebug, _log_name_+': '+'Set layout to %d columns and %d rows of keys', [cols, rows]);
  end;
end;

procedure TConfig.SetSystemLogThreshold(AValue: TLogThreshold);
const
  _log_name_ = 'TConfig.SystemLogThreshold';
begin
  if SystemLogThreshold = AValue then Exit;
  ulog.systemLogThreshold := AValue;
  FModified := true;
  LogOut(llDebug, _log_name_+': '+'Set to %s', [sLogThreshold[systemLogThreshold]]);
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

procedure LoadConfig;
begin
  LogOut(llDebug, 'LoadConfig');
  OnGetVendorName := @Vendor;
  OnGetApplicationName := @GetAppName;
  configdir := GetAppConfigDir(false);
  ForceDirectories(configdir);   // create config directory, report error if false ?
  configdir := IncludeTrailingPathDelimiter(configdir);
  configfile := configdir + OPTIONS_FILENAME;
  LogOut(llInfo, 'Configuration directory: %s', [configdir]);
  LogOut(llInfo, 'Configuration file: %s',  [OPTIONS_FILENAME]);
  config := TConfig.create;
  config.Init;
end;


initialization
  LoadConfig;
  LogOut(llDebug, 'uconfig initialization completed');

finalization
  freeandnil(config);
  LogOut(llDebug, 'uconfig finalization completed');
end.

