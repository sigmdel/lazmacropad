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
    Bevel3: TBevel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LogSizeSpinEdit: TSpinEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RadioButton1: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
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
    Label7: TLabel;
    RestoreConfigButton: TButton;
    SaveConfigButton: TButton;
    ColCountSpinEdit: TSpinEdit;
    RowCountSpinEdit: TSpinEdit;
    procedure ApplogThresholdButtonChange(Sender: TObject);
    procedure BaudComboBoxChange(Sender: TObject);
    procedure ConsoleLogLevelButtonsChange(Sender: TObject);
    procedure LogSizeSpinEditEditingDone(Sender: TObject);
    procedure SetButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DeviceEditEditingDone(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure SyslogThresholdButtonChange(Sender: TObject);
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
  main, ulog, uconfig, userial (*, serialreader, keymap;*);

{ TOptionsForm }

procedure TOptionsForm.ApplogThresholdButtonChange(Sender: TObject);
begin
  if Sender is TRadioButton then
    with Sender as TRadioButton do begin
      if checked then
        Config.AppLogThreshold := TLogLevel(tag);
    end;
end;

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
  end;
end;

procedure TOptionsForm.ConnectButtonClick(Sender: TObject);
begin
  MainForm.Timer1.Enabled := OpenSerial;
  MainForm.SetTrayIcon(MainForm.Timer1.Enabled);
end;

procedure TOptionsForm.ConsoleLogLevelButtonsChange(Sender: TObject);
begin
  if Sender is TRadioButton then
    with Sender as TRadioButton do begin
      if checked then
        Config.ConsoleLogThreshold := TLogLevel(tag);
    end;
end;

procedure TOptionsForm.DeviceEditEditingDone(Sender: TObject);
const
  _log_name_ = 'TOptionsForm.DeviceEditEditingDone';
begin
  if DeviceEdit.text <> Config.SerialDeviceName then begin
    CloseSerial;
    MainForm.SetTrayIcon(false);
    MainForm.Timer1.Enabled := false;
    Config.SerialDeviceName := DeviceEdit.text;
    LogOut(llInfo, _log_name_+': '+'New serial device: %s. Press [Connect] button to establish connection', [Config.SerialDeviceName]);
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

procedure TOptionsForm.LogSizeSpinEditEditingDone(Sender: TObject);
begin
  Config.AppLogSize := LogSizeSpinEdit.value;
  LogSizeSpinEdit.value := Config.AppLogSize; // just in case out of bounds
end;

procedure TOptionsForm.ResetButtonClick(Sender: TObject);
begin
  if (FOldCol > 0) and (FOldRow > 0) then begin
    ColCountSpinEdit.Value := FOldCol;
    RowCountSpinEdit.Value := FOldRow;
    SetButtonCount;
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

procedure TOptionsForm.SetButtonClick(Sender: TObject);
begin
  SetButtonCount;
end;

function TOptionsForm.SetButtonCount: boolean;
const
  _log_name_ = 'TOptionsForm.SetButtonCount';
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
      LogOut(llError, _log_name_+': '+'Must have at least one row and one column of buttons')
    else
      LogOut(llError, _log_name_+': '+'The maximum number of buttons is %d', [length(KeyLabels)]);
  end
  else begin
    FCountsChanged := false;
    (**************
    vis := LayoutForm.visible;
    freeandnil(LayoutForm);
    Application.CreateForm(TLayoutForm, LayoutForm);
    LayoutForm.visible := vis;
    mainForm.KeyLayoutItem.Checked := vis;
    *****)
    result := true;
    LogOut(llError, _log_name_+': '+'%d x %d button matrix defined', [Config.KeyRows, Config.KeyCols]);
  end;
end;

procedure TOptionsForm.SyslogThresholdButtonChange(Sender: TObject);
begin
  if Sender is TRadioButton then
    with Sender as TRadioButton do begin
      if checked then
        Config.SystemLogThreshold := TLogLevel(tag);
    end;
end;

procedure TOptionsForm.UpdateGUI;
begin
  DeviceEdit.Text := Config.SerialDeviceName;
  BaudComboBox.Text := inttostr(Config.Baud);

  RadioButton1.checked := Config.AppLogThreshold = llDebug;
  RadioButton2.checked := Config.AppLogThreshold = llInfo;
  RadioButton3.checked := Config.AppLogThreshold = llError;
  RadioButton4.checked := Config.AppLogThreshold = llNone;
  LogSizeSpinEdit.value := Config.AppLogSize;

  RadioButton5.checked := Config.ConsoleLogThreshold = llDebug;
  RadioButton6.checked := Config.ConsoleLogThreshold = llInfo;
  RadioButton7.checked := Config.ConsoleLogThreshold = llError;
  RadioButton8.checked := Config.ConsoleLogThreshold = llNone;

  RadioButton9.checked := Config.SystemLogThreshold = llDebug;
  RadioButton10.checked := Config.SystemLogThreshold = llInfo;
  RadioButton11.checked := Config.SystemLogThreshold = llError;
  RadioButton12.checked := Config.SystemLogThreshold = llNone;

  ColCountSpinEdit.value := Config.KeyCols;
  RowCountSpinEdit.value := Config.KeyRows;
end;

end.

