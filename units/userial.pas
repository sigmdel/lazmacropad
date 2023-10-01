unit userial;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure CloseSerial(quiet: boolean = false);
function OpenSerial: boolean;
function ReadSerial: char; // if #0 then no char available

  // returns -2 if never received data
  // returns -1 if time since last rx greater than SERIAL_LOST
  // otherwise returns seconds elapsed since last serial rx
function SecondsSinceLastRx: integer;

implementation

uses
  serial, uconfig, ulog (*, main*);

const
  BUFSIZE = 5;
  SERIAL_LOST = 20000;      // 4 times the Arduino keep alive SYN message interval
  LOG_SERIAL_LOST = 60000;  // log 'Keypad not connected' message interval

var
  serialhandle: longint;
  alivetime: QWORD = 0;
  disconnectTime: QWORD = 0;

function SecondsSinceLastRx: integer;
begin
  if alivetime = 0 then
    result := -2
  else begin
    result := integer (GetTickCount64 - alivetime);
    if result >= SERIAL_LOST then
      result := -1
    else
      result := result div 1000;
  end;
end;

procedure CloseSerial(quiet: boolean);
const
  _log_name_ = 'userial.CloseSerial';
begin
  if serialhandle > 0 then begin
    SerSync(serialhandle); // flush out any remaining before closure
    SerFlushOutput(serialhandle); // discard any remaining output
    SerClose(serialhandle);
  end;
  serialhandle := -1;
  if not quiet then
    LogOut(llDebug, _log_name_+': '+'Serial device closed');
  (****
  MainForm.SetTrayIcon(false);
  ***)
end;

function OpenSerial: boolean;
const
  _log_name_ = 'userial.OpenSerial';
var
  Flags: TSerialFlags; { set of (RtsCtsFlowControl); }
begin
  if serialhandle > 0 then
    CloseSerial;     // close the serial device if already opened
  serialhandle := SerOpen(Config.SerialDeviceName);
  if serialhandle > 0 then begin
    Flags := []; // none
    SerSetParams(serialhandle, Config.Baud, 8, NoneParity, 1, Flags);
    LogOut(llInfo,  _log_name_+': '+'Using device: %s at %d bps', [Config.SerialDeviceName, Config.Baud]);
    alivetime := GetTickCount64;
  end
  else if serialhandle = -1 then
    LogOut(llError, _log_name_+': '+'Serial device %s not found', [Config.SerialDeviceName])
  else
    LogOut(llError,  _log_name_+': '+'Unable to open serial device %s', [Config.SerialDeviceName]);
  result := serialhandle > 0;
end;

function ReadSerial: char;
const
  _log_name_ = 'userial.OpenSerial';
var
  inbuf: array[0..BUFSIZE-1] of char;
begin
  result := #0;
  if (serialhandle <= 0) then begin
    if (GetTickCount64 - disconnectTime > LOG_SERIAL_LOST) then begin
      LogOut(llInfo,  _log_name_+': '+'Keypad not connected');
      disconnectTime := GetTickCount64;
    end
  end
  else if SerRead(serialhandle, inbuf, sizeof(inbuf)) > 0 then begin
    result := inbuf[0];
    alivetime := GetTickCount64;
    SerFlushInput(serialhandle);  // flush everything
  end
  else begin
    if (GetTickCount64 - alivetime > SERIAL_LOST) then begin
        CloseSerial(true);
        LogOut(llError,  _log_name_+': '+'Serial connection with %s lost', [Config.SerialDeviceName]);
        LogOut(llError,  _log_name_+': '+'Use the [Connect] button in Parameters to reconnect');
        disconnectTime := GetTickCount64;
    end
  end;
end;

end.

