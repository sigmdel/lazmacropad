unit keymap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Params;

type

  { TLayoutForm }

  TLayoutForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
  public
    keys: array of TLabel;
    procedure UpdateGUI;
    procedure ButtonClick(Sender: TObject);
  end;

var
  LayoutForm: TLayoutForm;


function KeyLabelToInt(c: char): integer;

implementation

{$R *.lfm}

uses
  {$ifdef Windows}Windows,{$endif}
  main;

resourcestring
  SEmpty = 'not defined';

function KeyLabelToInt(c: char): integer;
begin
  result := pos(c, KeyLabels) - 1;
end;

{ TLayoutForm }

// BAD! this is hard coded for 4x4 keypad!
procedure TLayoutForm.FormCreate(Sender: TObject);
const
  SZ = 33;      // width & height of buttons
  DD = SZ + 12; // delta to next column / rows of buttons
var
  b: integer;     // button #
  x, y: integer;  // button left and top
  r, c: integer;  // button row, column
begin
  setlength(keys, config.ButtonCount);
  y := 0;
  b := 0;
  for r := 1 to config.KeyRows do begin
    x := 0;
    for c := 1 to config.KeyCols do begin
      keys[b] := TLabel.create(self);
      with keys[b] do begin
        parent := self;
        Left := x;
        Height := SZ;
        Top := y;
        Width := SZ;
        Alignment := taCenter;
        AutoSize := False;
        Caption := KeyLabels[b+1];
        Color := clSilver;
        Font.Color := clBlack;
        Font.Height := 16;
        Layout := tlCenter;
        ParentColor := False;
        ParentFont := False;
        ParentShowHint := False;
        ShowHint := True;
        Transparent := False;
        Tag := b;
        {$ifndef Windows}
        OnClick := @ButtonClick;
        {$endif}
      end;
      x := x + DD;
      inc(b);
    end;
    y := y + DD;
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

  //aPt := screen.WorkareaRect.bottomRight;
  aPt := screen.PrimaryMonitor.WorkareaRect.bottomRight;
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

{ #todo 5 -oMichel -cAppearance : This flashes LayoutForm; any way of avoiding this? }
{ #todo 2 -oMichel -cFunctionality : This does not work in Windows }
procedure TLayoutForm.ButtonClick(Sender: TObject);
begin
  {$ifndef Windows}
  if Sender is TLabel then with Sender as TLabel do begin
    if StringMacros[Tag] <> '' then begin
      self.Hide;  // return to previous focused window
      MainForm.Inject(tag);
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
  for i := 0 to  config.ButtonCount-1 do
    if StringMacros[i] = '' then
      LayoutForm.keys[i].Hint := SEmpty
    else
      LayoutForm.keys[i].Hint := StringMacros[i];
end;

end.

