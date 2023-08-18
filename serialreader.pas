unit serialreader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure CloseSerial(quiet: boolean = false);
function OpenSerial: boolean;
function ReadSerial: char; // if #0 then no char available

implementation

uses
  serial, macrolog, params, main;

const
  BUFSIZE = 5;
  SERIAL_LOST = 20000;      // 4 times the Arduino keep alive SYN message interval
  LOG_SERIAL_LOST = 60000;  // log 'Keypad not connected' message interval

var
  serialhandle: longint;
  alivetime: QWORD = 0;
  disconnectTime: QWORD = 0;

procedure CloseSerial(quiet: boolean);
begin
  if serialhandle > 0 then begin
    SerSync(serialhandle); // flush out any remaining before closure
    SerFlushOutput(serialhandle); // discard any remaining output
    SerClose(serialhandle);
  end;
  serialhandle := -1;
  if not quiet then
    LogForm.Log(llInfo, 'Serial device closed');
  MainForm.SetTrayIcon(false);
end;

function OpenSerial: boolean;
var
  Flags: TSerialFlags; { set of (RtsCtsFlowControl); }
begin
  if serialhandle > 0 then
    CloseSerial;     // close the serial device if already opened
  serialhandle := SerOpen(Config.DeviceName);
  if serialhandle > 0 then begin
    Flags := []; // none
    SerSetParams(serialhandle, Config.Baud, 8, NoneParity, 1, Flags);
    LogForm.Log(llInfo, 'Using device: %s at %d bps', [Config.DeviceName, Config.Baud]);
    alivetime := GetTickCount64;
  end
  else if serialhandle = -1 then
    LogForm.Log(llError, 'Serial device %s not found', [Config.DeviceName])
  else
    LogForm.Log(llError, 'Unable to open serial device %s', [Config.DeviceName]);
  result := serialhandle > 0;
end;

function ReadSerial: char;
var
  inbuf: array[0..BUFSIZE-1] of char;
begin
  result := #0;
  if (serialhandle <= 0) then begin
    if (GetTickCount64 - disconnectTime > LOG_SERIAL_LOST) then begin
      LogForm.Log(llInfo, 'Keypad not connected');
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
        LogForm.Log(llError, 'Serial connection with %s lost', [Config.DeviceName]);
        LogForm.Log(llError, 'Use the [Connect] button in Parameters to reconnect');
        disconnectTime := GetTickCount64;
    end
  end;
end;

end.

