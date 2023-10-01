unit ulog;

(*
 * Unit to allow simultaneous logging to the application log, the
 * console (stdout) and to the system log.
 *
 * This shim accesses macrolog indirectly. In futur versions it should be
 * relatively easy to remove the app log system with minimal change to
 * the code.
 *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogThreshold = (llDebug, llInfo, llError, llNone);
  TLogLevel = llDebug..llError;



  // These variables are set with default values below and are then
  // updated when the configuration file is read.
  // They should be modified only through the Config class properties
  // defined in uconfig.
var
  appLogSize: integer = 256;                    // keep only latest 256 message in app log
  appLogThreshold: TLogThreshold = llNone;      // usual to show info+errors, but llDebug is useful during development
  consoleLogThreshold: TLogThreshold = llDebug; // always publish everything to console at start
  systemLogThreshold: TLogThreshold = llError;  // usual to show only errors or errors+info in syslog

procedure LogOut(level: TLogLevel; const msg: string);
procedure LogOut(level: TLogLevel; const msg: string; args: array of const);

const
  sLogThreshold: array[TLogThreshold] of string = (
    'Debug', 'Information', 'Error', 'None');

implementation

uses
  uconfig, {$ifndef UNIT_TESTS} macrolog, {$endif} eventlog;

const
  SLogLevel: array[TLogLevel] of string = ('DBG', 'INF', 'ERR');

var
  systemlog: TEventLog = nil;

procedure LogOut(level: TLogLevel; const msg: string);
var
  lv: TLogThreshold absolute level;
begin
  {$ifndef WINDOWS}
  if level >= consoleLogThreshold then
    //Type   Range
    //QWord  0 .. 18446744073709551615
    //            12345678901234567890
    writeln(Format('%.20d [%s] %s', [GetTickCount64, SLoglevel[level], msg]));
  {$endif}

  if lv >= systemLogThreshold then begin
    if not assigned(systemlog) then begin
      systemlog := TEventLog.create(nil);
      systemlog.LogType := ltSystem;
    end;
    if assigned(systemlog) then
      case lv of
        lldebug: systemlog.Debug(msg);
        llinfo: systemlog.Info(msg);
        llerror: systemlog.Error(msg) ;
      end;
  end;

  {$ifndef UNIT_TESTS}
  if (lv >= appLogThreshold) then
    appLog(Format('[%s] %s', [SLoglevel[level], msg]));
  {$endif}
end;

procedure LogOut(level: TLogLevel; const msg: string; args: array of const);
begin
  LogOut(level, Format(msg, args));
end;

procedure LogWarning;
var
  oldlevel: TLogThreshold;
begin
  if AppLogThreshold >= llError then begin
    oldlevel := AppLogThreshold;
    AppLogThreshold := llInfo;
    LogOut(llInfo, 'Change the app log threshold in parameters to see more information.');
    AppLogThreshold := oldlevel;
  end;
end;

initialization
LogWarning;
LogOut(llDebug, 'ulog initialization completed');

finalization
  if assigned(systemlog) then
    systemlog.free;
  LogOut(llDebug, 'ulog finalization completed');
end.
