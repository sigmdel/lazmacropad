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
  serial, macrolog, params;

const
  BUFSIZE = 5;

var
  serialhandle: longint;

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
  end
  else if serialhandle = -1 then
    LogForm.Log(llError, 'Serial device %s not found', [Config.DeviceName])
  else
    LogForm.Log(llError, 'Unable to open serial device %s', [Config.DeviceName]);
  result := serialhandle > 0;
end;


function ReadSerial: char;
var
  count: integer;
  inbuf: array[0..BUFSIZE-1] of char;
  {$ifndef WINDOWS}
  sstate: TSerialState;
  {$endif}
begin
  result := #0;
  if (serialhandle <= 0) then begin
    LogForm.Log(llError, 'Serial device %s not connected', [Config.DeviceName])
  end
  else begin
    count := SerRead(serialhandle, inbuf, sizeof(inbuf));
    if count > 0 then begin
      result := inbuf[0];
      SerFlushInput(serialhandle);  // flush everything
    end
    else begin
      // nothing read, check if connection still valid
      {$IFNDEF WINDOWS}
      // unable to check if connection lost in Windows
      // could have the nano send a 'heartbeat' message
      // (say '@') at regular intervals and use that to
      // check for lost connections
      // nothing read, check if connection still valid
      sstate := SerSaveState(serialhandle);
      if sstate.LineState = 0 then begin
        // serial connection lost
        CloseSerial(true);
        LogForm.Log(llError, 'Serial connection with %s lost (LineState = %d)', [ Config.DeviceName, sstate.LineState]);
        LogForm.Log(llError, 'Use the [Connect] button in Parameters to reconnect');
      end;
      {$ENDIF}
    end
  end;
end;


end.

