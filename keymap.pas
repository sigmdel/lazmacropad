unit keymap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Params;

type

  { TLayoutForm }

  TLayoutForm = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
  public
    keys: array[0..BUTTON_COUNT-1] of TLabel;
    procedure UpdateGUI;
    procedure ButtonClick(Sender: TObject);
  end;

var
  LayoutForm: TLayoutForm;

implementation

{$R *.lfm}

uses
  {$ifdef Windows}Windows,{$endif}
  main;

{ TLayoutForm }

// BAD! this is hard coded for 4x4 keypad!
procedure TLayoutForm.FormCreate(Sender: TObject);
const
  SZ = 33; // width & height of buttons
  DX = SZ + 20;  // delta to next button
  MX = 32;
  MY = 12;
var
  i: integer;     // button #
  x, y: integer;  // left and top

begin
  x := MX;
  y := MY-DX;
  for i := 0 to 15 do begin
    if i mod 4 = 0 then begin
      x := MX;
      y := y + DX;
    end;
    keys[i] := TLabel.create(self);
    with keys[i] do begin
      parent := self;
      Left := x;
      Height := SZ;
      Top := y;
      Width := SZ;
      Alignment := taCenter;
      AutoSize := False;
      Caption := inttohex(i, 1);
      Color := clSilver;
      Font.Color := clBlack;
      Font.Height := 16;
      Layout := tlCenter;
      ParentColor := False;
      ParentFont := False;
      ParentShowHint := False;
      ShowHint := True;
      Transparent := False;
      Tag := i;
      {$ifndef Windows}
      OnClick := @ButtonClick;
      {$endif}
    end;
    x := x + DX;
  end;
end;

procedure TLayoutForm.FormHide(Sender: TObject);
begin
  MainForm.KeyLayoutItem.Checked := false;
end;

{ #todo 2 -oMichel -cAppearance : This assumes taskbar is at top or bottom. What if is is at right or left  }
procedure TLayoutForm.FormShow(Sender: TObject);
var
  aPt: TPoint;
begin
  //top := screen.Height - height;
  //left := screen.Width - width;
  // above won't work with Windows taskbar

  //aPt := screen.PrimaryMonitor.WorkareaRect.bottomRight;
  aPt := screen.WorkareaRect.bottomRight;
  top := aPt.y - height
  {$ifdef Windows}
    - GetSystemMetrics(SM_CYSIZE) - GetSystemMetrics(SM_CYSIZEFRAME);
  {$endif};
  left := aPt.x - width;
  UpdateGUI;
end;

procedure TLayoutForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
end;

{ #todo 5 -oMichel -cAppearance : This flashes LayoutForm; anyway of avoiding this? }
{ #todo 2 -oMichel -cFunctionality : This does not work in Windows }
procedure TLayoutForm.ButtonClick(Sender: TObject);
begin
  {$ifndef Windows}
  if Sender is TLabel then with Sender as TLabel do begin
    if Macros[Tag] <> '' then begin
      self.Hide;  // return to previous focused window
      MainForm.Inject(macros[tag]);
      self.Show;  // show this form again
      MainForm.KeyLayoutItem.Checked := false;
    end;
  end;
  {$endif}
end;

procedure TLayoutForm.UpdateGUI;
var
  i: integer;
begin
  for i := 0 to  BUTTON_COUNT-1 do
    LayoutForm.keys[i].Hint := macros[i];
end;

end.

