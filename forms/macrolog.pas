unit macrolog;

(*
 * Application log window.
 *
 * This is useful while developping the application but it could
 * probably be removed in a final version. Using the console
 * and system log facilities should be sufficient once a better
 * feedback is provided about the connection status
 *)

{$mode ObjFPC}{$H+}

{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType;

type

  { TLogForm }

  TLogForm = class(TForm)
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
  end;

var
  LogForm: TLogForm;

procedure AppLog(const msg: string);

implementation

{$R *.lfm}

uses
  main, ulog;

var
  templog: TStrings = nil; // Buffer for log messages until the log form is created

procedure TLogForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Width := 2*Width;
end;

procedure TLogForm.FormHide(Sender: TObject);
begin
  MainForm.LogItem.Checked := false;
end;

procedure TLogForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  // show any buffered messages and shut the buffer down
  if assigned(templog) then begin
    for i := 0 to templog.count-1 do
      logMemo.Lines.Add(templog[i]);
    freeandnil(templog);
  end;
end;

procedure TLogForm.LogMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if (Key = VK_INSERT) and (Shift = [ssShift]) then
      Key := 0
    else if (Key = VK_V) and (Shift = [ssCtrl]) then
      Key := 0;
end;

procedure appLog(const msg: string);
begin
  if application.Terminated then
    exit; // LogForm is already destroyed, there's no LogMemo

  if assigned(logForm) then with logForm do begin
    while LogMemo.Lines.Count >= ulog.AppLogSize do
      LogMemo.Lines.Delete(0);
    LogMemo.Lines.Add(msg);
    LogMemo.SelStart := length(LogMemo.Text);
  end
  else begin
    if not assigned(templog) then
      templog := TStringList.create;
    templog.add(msg);
  end;
end;


initialization
  LogOut(llDebug, 'macrolog initialization');
end.

