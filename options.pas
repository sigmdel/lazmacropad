unit options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    BaudComboBox: TComboBox;
    Bevel1: TBevel;
    Bevel3: TBevel;
    ConnectButton: TButton;
    DeviceEdit: TEdit;
    DisconnectButton: TButton;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RestoreConfigButton: TButton;
    SaveConfigButton: TButton;
    procedure BaudComboBoxChange(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DeviceEditEditingDone(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure LogLevelButtonsChange(Sender: TObject);
    procedure RestoreConfigButtonClick(Sender: TObject);
    procedure SaveConfigButtonClick(Sender: TObject);
  private
    procedure UpdateGUI;
  public
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses
  main, macrolog, serialreader, params;

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
  //UpdateGui;
end;

procedure TOptionsForm.SaveConfigButtonClick(Sender: TObject);
begin
  Config.Save;
end;

procedure TOptionsForm.UpdateGUI;
begin
  DeviceEdit.Text := Config.DeviceName;
  BaudComboBox.Text := inttostr(Config.Baud);
  RadioButton1.checked := Config.logLevel = llDebug;
  RadioButton2.checked := Config.logLevel = llInfo;
  RadioButton3.checked := Config.logLevel = llError;
end;

end.

