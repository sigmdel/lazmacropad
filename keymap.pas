unit keymap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Params;

type

  { TLayoutForm }

  TLayoutForm = class(TForm)
    Image1: TImage;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
  public
    keys: array[0..BUTTON_COUNT-1] of TLabel;
  end;

var
  LayoutForm: TLayoutForm;

implementation

{$R *.lfm}

uses main;

{ TLayoutForm }

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
      //Hint := '<a href:="" target:="_blank">';
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
    end;
    x := x + DX;
  end;
end;

procedure TLayoutForm.FormShow(Sender: TObject);
begin
   if MainForm.visible then begin
     Top := MainForm.Top;
     Left := MainForm.Left + MainForm.Width;
   end;
end;

procedure TLayoutForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
end;

procedure TLayoutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MainForm.ShowKeysCheckBox.checked := false;;
end;


end.

