unit macrolog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  params;

type

  { TLogForm }

  TLogForm = class(TForm)
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure LogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private

  public
    procedure Log(level: TLogLevel; const msg: string);
    procedure Log(level: TLogLevel; const msg: string; args: array of const);
  end;

var
  LogForm: TLogForm;

implementation

{$R *.lfm}

uses
  main;


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

procedure TLogForm.Log(level: TLogLevel; const msg: string);
begin
  if level >= Config.logLevel then begin
    LogMemo.Lines.Add(msg);
    LogMemo.SelStart := length(LogMemo.Text);
  end;
end;

procedure TLogForm.Log(level: TLogLevel; const msg: string; args: array of const);
begin
  Log(level, Format(msg, args));
end;

procedure TLogForm.LogMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if (Key = VK_INSERT) and (Shift = [ssShift]) then
      Key := 0
    else if (Key = VK_V) and (Shift = [ssCtrl]) then
      Key := 0;
end;

end.

