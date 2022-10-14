unit params;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

CONST
  BUTTON_COUNT = 16;
  OPTIONS_FILENAME = 'options.ini';


type
  TLogLevel = (llDebug, llInfo, llError, llNone);

var
  Macros: array[0..BUTTON_COUNT-1] of string;
  MacrosModified: boolean;

procedure SaveMacros(const filename: string);
procedure LoadMacros(const filename: string);

type
  TConfig = class
  private
    FDeviceName: string;
    FBaud: longint;
    FLoglevel: TLogLevel;
    FDefaultMacrosFile: string;
    FModified: boolean;
    procedure SetBaud(AValue: longint);
    procedure SetDefaultMacrosFile(const AValue: string);
    procedure SetDeviceName(AValue: string);
    procedure SetLogLevel(AValue: TLogLevel);
  public
    constructor Create;
    procedure Save;
    procedure Load;
    property Baud: longint read FBaud write SetBaud;
    property DefaultMacrosFile: string read FDefaultMacrosFile write SetDefaultMacrosFile;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property LogLevel: TLogLevel read FLogLevel write SetLogLevel;
    property Modified: boolean read FModified write FModified;
  end;

var
  Config: TConfig;
  ConfigDir: string;

procedure ParamsInit;

implementation

uses
  inifiles, main;

var
  configfile: string;


procedure SaveMacros(const filename: string);
var
  i,n: integer;
  s: string;
begin
  n := 0;
  with TInifile.create(filename) do try
     for i := 0 to BUTTON_COUNT-1 do begin
        WriteString('macros', inttohex(i,2), macros[i]);
        if macros[i] <> '' then inc(n);
     end;
     MacrosModified := false;
     Form1.log(llInfo,'Macros file %s with %d macro definitions saved', [filename, n]);
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
  for i := 0 to BUTTON_COUNT-1 do
     macros[i] := '';

  n := 0;
  if (filename = '') then
    Form1.log(llError, 'params.LoadMacros() called with empty filename')
  else if not fileexists(filename) then
    Form1.log(llError, 'Macros file cannot be loaded, %s does not exist', [filename])
  else with TInifile.create(filename) do try
     for i := 0 to BUTTON_COUNT-1 do begin
        s := ReadString('macros', inttohex(i,2), macros[i]);
        if s <> '' then begin
          inc(n);
          macros[i] := s;
        end;
     end;
     Form1.log(llInfo,'Macros file %s contained %d macro definitions', [filename, n]);
  finally
    free;
  end;
end;


{ TConfig }

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

constructor TConfig.Create;
begin
  {$ifdef LINUX}
  FDeviceName := '/dev/ttyUSB0';
  {$endif}
  {$ifdef WINDOWS}
  FDeviceName := 'COM1';
  {$endif}
  FBaud := 9600;
  FLogLevel := llInfo;
  FDefaultMacrosFile := '';
end;

procedure TConfig.Save;
begin
  with TInifile.create(configfile) do try
     WriteString('serial', 'device', FDeviceName);
     WriteInteger('serial', 'baud', FBaud);
     WriteInteger('log', 'level', ord(FLoglevel));
     writeString('macros', 'filename', FDefaultMacrosFile);
     FModified := false;
   finally
     free;
   end;
   Form1.log(llInfo, 'Configuration file %s saved', [configfile]);
end;

procedure TConfig.Load;
begin
  if not fileexists(configfile) then begin
    // make sure a config file always exists
    Save;
    Form1.log(llInfo, 'Default configuration file created because none was found');
    exit;
  end;
  with TInifile.create(configfile) do try
    FDeviceName := ReadString('serial', 'device', FDeviceName);
    FBaud := ReadInteger('serial', 'baud', FBaud);
    Floglevel := TLogLevel(ReadInteger('log', 'level', ord(Floglevel)));
    FDefaultMacrosFile := ReadString('macros', 'filename', FDefaultMacrosFile);
    if not fileexists(FDefaultMacrosfile) and fileexists(configdir+FDefaultMacrosfile) then
       FDefaultMacrosfile := configdir+FDefaultMacrosfile;
    FModified := false;
  finally
    free;
  end;
  Form1.log(llInfo, 'Configuration file %s loaded', [configfile]);
end;

function Vendor: string;
begin
  result := 'sigmdel';
end;

function GetAppName: string;
begin
  result := changefileext(extractfilename(paramstr(0)), '');
end;


// Can't do this in initialization because Form1.log is used
// in config.Load which thus requires mainunit.Form1 to be created
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
  LoadMacros(Config.DefaultMacrosfile);
end;

finalization
  config.free;
end.

