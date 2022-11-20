unit options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    BaudComboBox: TComboBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label8: TLabel;
    SetButton: TButton;
    ResetButton: TButton;
    ConnectButton: TButton;
    DeviceEdit: TEdit;
    DisconnectButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RestoreConfigButton: TButton;
    SaveConfigButton: TButton;
    ColCountSpinEdit: TSpinEdit;
    RowCountSpinEdit: TSpinEdit;
    LogSizeSpinEdit: TSpinEdit;
    procedure BaudComboBoxChange(Sender: TObject);
    procedure LogSizeSpinEditEditingDone(Sender: TObject);
    procedure SetButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DeviceEditEditingDone(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure LogLevelButtonsChange(Sender: TObject);
    procedure RestoreConfigButtonClick(Sender: TObject);
    procedure RowCountSpinEditEditingDone(Sender: TObject);
    procedure SaveConfigButtonClick(Sender: TObject);
  private
    FOldCol: integer;
    FOldRow: integer;
    FCountsChanged: boolean;
    function SetButtonCount: boolean;
    procedure UpdateGUI;
  public
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses
  main, macrolog, serialreader, params, keymap;

{ TOptionsForm }

procedure TOptionsForm.BaudComboBoxChange(Sender: TObject);
var
  newbaud: longint;
begin
  newbaud := strtointdef(BaudComboBox.Text, Config.Baud);
  if newbaud <> Config.Baud then begin
     MainForm.Timer1.Enabled := false;
     CloseSerial;
     MainForm.SetTrayIcon(false);
     Config.Baud := newbaud;
     //Log(llInfo, 'New baud, %d. Connection to %s closed. Press [Connect] button to establish connection.', [Baud, DeviceName]);
     LogForm.Log(llInfo, 'New baud, %d. Press [Connect] button to establish connection with this new value.', [Config.Baud]);
  end;
end;

procedure TOptionsForm.LogSizeSpinEditEditingDone(Sender: TObject);
begin
  Config.LogSize := LogSizeSpinEdit.value;
  LogSizeSpinEdit.value := Config.LogSize; // just in case out of bounds
end;

procedure TOptionsForm.SetButtonClick(Sender: TObject);
begin
  SetButtonCount;
end;

procedure TOptionsForm.ResetButtonClick(Sender: TObject);
begin
  if (FOldCol > 0) and (FOldRow > 0) then begin
    ColCountSpinEdit.Value := FOldCol;
    RowCountSpinEdit.Value := FOldRow;
    SetButtonCount;
  end;
end;

procedure TOptionsForm.ConnectButtonClick(Sender: TObject);
begin
  if OpenSerial then begin
    MainForm.Timer1.Enabled := true;
    MainForm.SetTrayIcon(true);
  end;
end;

procedure TOptionsForm.DeviceEditEditingDone(Sender: TObject);
begin
  if DeviceEdit.text <> Config.DeviceName then begin
    CloseSerial;
    MainForm.SetTrayIcon(false);
    MainForm.Timer1.Enabled := false;
    Config.DeviceName := DeviceEdit.text;
    LogForm.Log(llInfo, 'New serial device, %s, not opened. Press [Connect] button to establish connection', [Config.DeviceName]);
  end;
end;

procedure TOptionsForm.DisconnectButtonClick(Sender: TObject);
begin
  MainForm.Timer1.Enabled := false;
  CloseSerial;
  MainForm.SetTrayIcon(false);
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TOptionsForm.FormHide(Sender: TObject);
begin
  MainForm.OptionsItem.Checked := false;
end;

procedure TOptionsForm.LogLevelButtonsChange(Sender: TObject);
begin
  if Sender is TRadioButton then with Sender as TRadioButton do begin
    if (checked and (Config.loglevel <> TLogLevel(tag)) ) then begin
      Config.loglevel := TLogLevel(tag);
    end;
  end;
end;

procedure TOptionsForm.RestoreConfigButtonClick(Sender: TObject);
begin
  Config.Load;
  UpdateGui;
  SetButtonCount;
end;

procedure TOptionsForm.RowCountSpinEditEditingDone(Sender: TObject);
begin
  FCountsChanged := (RowCountSpinEdit.Value <> Config.KeyRows)
    or (ColCountSpinEdit.Value <> Config.KeyCols);
end;

procedure TOptionsForm.SaveConfigButtonClick(Sender: TObject);
begin
  if FCountsChanged and not SetButtonCount then
    exit;
  Config.Save;
end;

function TOptionsForm.SetButtonCount: boolean;
var
  r, c: integer;
  vis: boolean;
begin
  r := RowCountSpinEdit.Value;
  c := ColCountSpinEdit.Value;
  if (r = Config.KeyRows) and (c = Config.KeyCols) then begin
    FCountsChanged := false;
    result := true;
  end;
  FOldCol := Config.KeyCols;
  FOldRow := Config.KeyRows;
  ResetButton.Hint := Format('%d x %d', [FOldCol, FOldRow]);
  if not Config.SetKeyLayout(c, r) then begin
    result := false;
    FCountsChanged := true;
    if (c < 1) or (r < 1) then
      LogForm.Log(llError, 'Must have at least one row and one column of buttons')
    else
      LogForm.Log(llError, 'The maximum number of buttons is %d', [length(KeyLabels)]);
  end
  else begin
    FCountsChanged := false;
    vis := LayoutForm.visible;
    freeandnil(LayoutForm);
    Application.CreateForm(TLayoutForm, LayoutForm);
    LayoutForm.visible := vis;
    mainForm.KeyLayoutItem.Checked := vis;
    result := true;
  end;
end;

procedure TOptionsForm.UpdateGUI;
begin
  DeviceEdit.Text := Config.DeviceName;
  BaudComboBox.Text := inttostr(Config.Baud);
  RadioButton1.checked := Config.logLevel = llDebug;
  RadioButton2.checked := Config.logLevel = llInfo;
  RadioButton3.checked := Config.logLevel = llError;
  LogSizeSpinEdit.value := Config.LogSize;
  ColCountSpinEdit.value := Config.KeyCols;
  RowCountSpinEdit.value := Config.KeyRows;
end;

end.

