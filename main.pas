unit main;

{$mode objfpc}{$H+}

interface

///// Attention:
/////   DEBUG and look at lost memory in Linux!!!

{ DEFINE TEST_SHIFT_INSERT}
{ If define Shift+Insert will be used as the clipboard paste shortcut.
  FOR TESTING ONLY!
  Do not do this, in all likelyhood it will not give the desired result
}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, clipbrd,
  LCLIntf, LMessages, LCLType, ComCtrls, ExtCtrls, ValEdit, EditBtn, Buttons,
  Grids, MouseAndKeyInput, serial, params;

{ https://wiki.lazarus.freepascal.org/MouseAndKeyInput
  MouseAndKeyInput

  See Usage in above or the following Forum entry

    https://forum.lazarus.freepascal.org/index.php/topic,56015.msg416307.html#msg416307
    You must add the lazmouseandkeyinput package to the requirements of your project
    ("Project" > "Project inspector" >"Add" > "New Requirement") in order to get
    access to the MouseAndKeyInput unit.
}

const
  BUFSIZE = 5;
  LM_NEW_MACROFILENAME = LM_USER + 1;          // LM_USER = WM_USER
  LM_SAVE_MACROS_QUITTING = LM_NEW_MACROFILENAME + 1;

type
  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel3: TBevel;
    AboutButton: TButton;
    ShowKeysCheckBox: TCheckBox;
    MacrosFileNameEdit: TFileNameEdit;
    RestoreConfigButton: TButton;
    DefaultCheckbox: TCheckBox;
    CloseButton: TButton;
    Label6: TLabel;
    Label2: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    SaveConfigButton: TButton;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    SaveDialog1: TSaveDialog;
    SaveMacrosButton: TButton;
    BaudComboBox: TComboBox;
    DeviceEdit: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LogMemo: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    MacrosEditor: TStringGrid;
    procedure AboutButtonClick(Sender: TObject);
    procedure BaudComboBoxEditingDone(Sender: TObject);
    procedure LogMemoKeyPress(Sender: TObject; var Key: char);
    procedure MacrosEditorSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure MacrosFileNameEditAcceptFileName(Sender: TObject;
      var Value: String);
    procedure MacrosFileNameEditEditingDone(Sender: TObject);
    procedure MacrosFileNameEditEnter(Sender: TObject);
    procedure RestoreConfigButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DefaultCheckboxChange(Sender: TObject);
    procedure DeviceEditEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MacrosEditorEditingDone(Sender: TObject);
    procedure LogLevelRadioButtonsChange(Sender: TObject);
    procedure SaveMacrosButtonClick(Sender: TObject);
    procedure SaveConfigButtonClick(Sender: TObject);
    procedure ShowKeysCheckBoxChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    serialhandle: LongInt;
    currentmacrosfile: string;
    procedure Callback(const src: string);
    function OpenSerialDevice: boolean;
    procedure CloseSerialDevice(quiet: boolean = false);
    procedure Inject(const macro: string);
    procedure UpdateGUI;
  public
    procedure NewMacroFileName(var Msg: TLMessage); message LM_NEW_MACROFILENAME;
    procedure SaveMacrosQuitting(var Msg: TLMessage); message LM_SAVE_MACROS_QUITTING;
    procedure Log(level: TLogLevel; const msg: string);  overload;
    procedure Log(level: TLogLevel; const msg: string; args: array of const);  overload;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  about, keymap;

{ TMainForm }

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  aboutform.showmodal;
end;

procedure TMainForm.BaudComboBoxEditingDone(Sender: TObject);
var
  newbaud: longint;
begin
  newbaud := strtointdef(BaudComboBox.Text, Config.Baud);
  if newbaud <> Config.Baud then begin
     //CloseSerialDevice;
     Config.Baud := newbaud;
     //Log(llInfo, 'New baud, %d. Connection to %s closed. Press [Connect] button to establish connection.', [Baud, DeviceName]);
     Log(llInfo, 'New baud, %d. Press [Connect] button to establish connection with this new value.', [Config.Baud]);
  end;
end;

procedure TMainForm.Callback(const src: string);
var
  i: integer;
begin
  Log(llDebug, 'callback(%s)', [src]);
  i := strtointdef('$'+src, -1);
  if (i < 0) or (i >= BUTTON_COUNT) then
    exit;
  if (macros[i] <> '') then begin
    inject(macros[i]);
    Log(llInfo, 'Key: %s, Macro: %s', [src, macros[i]]);
  end
  else
    Log(llInfo, 'Key: %s, no macro defined', [src]);
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.CloseSerialDevice(quiet: boolean);
begin
  if serialhandle > 0 then begin
    SerSync(serialhandle); // flush out any remaining before closure
    SerFlushOutput(serialhandle); // discard any remaining output
    SerClose(serialhandle);
  end;
  serialhandle := -1;
  Timer1.Enabled := false;
  if not quiet then
    Log(llInfo, 'Closing serial device');
end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  OpenSerialDevice;
end;

procedure TMainForm.DefaultCheckboxChange(Sender: TObject);
begin
  if DefaultCheckbox.checked then begin
     if MacrosFileNameEdit.filename = '' then begin
        DefaultCheckbox.checked := false;
        exit;
     end;
     Config.DefaultMacrosFile := MacrosFileNameEdit.Filename;
  end
  else if (MacrosFileNameEdit.Filename = Config.DefaultMacrosFile) then
    DefaultCheckbox.checked := true;
end;

procedure TMainForm.DeviceEditEditingDone(Sender: TObject);
begin
  if DeviceEdit.text <> Config.DeviceName then begin
     //CloseSerialDevice;
     Config.DeviceName := DeviceEdit.text;
     LogMemo.lines.add('New serial device, %s, not opened. Press [Connect] button to establish connection', [Config.DeviceName]);
  end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  MacrosFileNameEdit.Filename := Config.DefaultMacrosFile;
  OpenSerialDevice;
  SaveDialog1.InitialDir := configdir;
  UpdateGUI;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled := false;
  CloseSerialDevice(true);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  mr: TModalResult;
  x, y: integer;
begin
  x := left + 100;
  y := top + 100;
  CanClose := false;  // check if the
  if macrosmodified then begin
    mr := MessageDlgPos('Save the modified macro definitions before closing.',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0, x, y);
    if (mr = mrYes) then begin
      PostMessage(self.handle, LM_SAVE_MACROS_QUITTING, 0, 0);
      exit;
    end;
    if (mr = mrCancel) then
      exit;
  end;
  if Config.Modified then begin
    mr := MessageDlgPos('Save the modified configuration before closing.',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0, x, y);
    if mr = mrYes then
      Config.Save;
    if (mr = mrCancel) then
      exit;
  end;
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  serialhandle := 0;  // not serial device opened
  constraints.MinWidth := width;
  constraints.MinHeight := height;
  ParamsInit;
  log(llError, 'Form minimum height x width: %d x %d px', [height, width]);
  Caption := changefileext(extractfilename(application.exename), '');
  MacrosEditor.RowCount := BUTTON_COUNT+1;
  for i := 0 to BUTTON_COUNT-1 do
    MacrosEditor.Cells[0,i+1] := inttohex(i, 1);
end;

procedure TMainForm.Inject(const macro: string);
begin
  // KeyInput.Press(macro); will not work with mapped keyboards
  // For example
  //    macro = 1234567890
  // will send  &é"'(-è_çà  to the focused application
  // if an FR (AZERTY) keyboard is used because KeyInput
  // emulates scan codes from the keyboard which are then
  // converted to the FR characters before being passed on
  // to the application
  clipboard.AsText := macro;

{$IFDEF TEST_SHIFT_INSERT}
  // Inject Shift-Insert (paste) keyboard shortcut
  KeyInput.Apply([ssShift]);
  KeyInput.Press(VK_INSERT);
  KeyInput.Unapply([ssShift]);
{$ELSE}
  // Inject ^V (paste) keyboard shortcut
  KeyInput.Apply([ssCtrl]);
  KeyInput.Press(VK_V);
  KeyInput.Unapply([ssCtrl]);
{$ENDIF}
  //clipboard.Clear;  // doest not seem to do anything at least with Diodon
end;

procedure TMainForm.Log(level: TLogLevel; const msg: string);
begin
  if level >= Config.logLevel then begin
    LogMemo.Lines.Add(msg);
    LogMemo.SelStart := length(LogMemo.Text);
  end;
end;

procedure TMainForm.Log(level: TLogLevel; const msg: string; args: array of const);
begin
  Log(level, Format(msg, args));
end;

procedure TMainForm.LogLevelRadioButtonsChange(Sender: TObject);
begin
  if Sender is TRadioButton then with Sender as TRadioButton do begin
    if (checked and (Config.loglevel <> TLogLevel(tag)) ) then begin
      Config.loglevel := TLogLevel(tag);
      TabSheet1.TabVisible := Config.loglevel <> llNone;
    end;
  end;
end;

(*
If Shift+Insert and Ctrl+V are to be used as keyboard shortcuts to paste
the content of the clipboard, then KeyDown event should be used to
prevent pasting to the LogMemo instead of just KeyPress.

procedure TMainForm.LogMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if (Key = VK_INSERT) and (Shift = [ssShift]) then
      Key := 0
    else if (Key = VK_V) and (Shift = [ssCtrl]) then
      Key := 0;
end;
*)

procedure TMainForm.LogMemoKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #22 then Key := #0;  // stop Ctrl+V from working in log
end;

procedure TMainForm.MacrosEditorEditingDone(Sender: TObject);
begin
  if (MacrosEditor.Row >= 1) and (MacrosEditor.Row <= BUTTON_COUNT) and
  (macros[MacrosEditor.Row-1] <> MacrosEditor.Cells[1, MacrosEditor.Row]) then begin
     macros[MacrosEditor.Row-1] := MacrosEditor.Cells[1, MacrosEditor.Row];
     macrosmodified := true;
     SaveMacrosButton.Enabled := true;
     LayoutForm.keys[MacrosEditor.Row-1].Hint := macros[MacrosEditor.Row-1];
  end;
end;

procedure TMainForm.MacrosEditorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (aCol > 0) and (aRow > 0);
end;

procedure TMainForm.MacrosFileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  PostMessage(self.handle, LM_NEW_MACROFILENAME, 0, 0);
  // use post to return immediately so that MacrosFilenameEdit.Filename
  // get set to Value before MacrosFileNameEditEditingDone is
  // called by the message handler NewMacroFileName(var Msg: TLMessage)
end;

procedure TMainForm.MacrosFileNameEditEditingDone(Sender: TObject);
begin
  if MacrosFileNameEdit.filename <> currentMacrosFile then begin
    LoadMacros(MacrosFileNameEdit.filename);
    currentMacrosFile := MacrosFileNameEdit.filename;
    UpdateGUI;
  end;
end;

procedure TMainForm.MacrosFileNameEditEnter(Sender: TObject);
begin
  currentmacrosfile := MacrosFileNameEdit.filename;
end;

procedure TMainForm.NewMacroFileName(var Msg: TLMessage);
begin
  MacrosFileNameEditEditingDone(nil);
end;

function TMainForm.OpenSerialDevice: boolean;
var
  Flags: TSerialFlags; { set of (RtsCtsFlowControl); }
begin
  Timer1.Enabled := false; // assume this will not work
  if serialhandle > 0 then
    CloseSerialDevice;     // close the serial device if already opened

  serialhandle := SerOpen(Config.DeviceName);
  if serialhandle > 0 then begin
    Flags := []; // none
    SerSetParams(serialhandle, Config.Baud, 8, NoneParity, 1, Flags);
    Log(llInfo, 'Using device: %s at %d bps', [Config.DeviceName, Config.Baud]);
    Timer1.Enabled := true;  // start reading from the serial device
  end
  else if serialhandle = -1 then
    Log(llError, 'Serial device %s not found', [Config.DeviceName])
  else
    Log(llError, 'Unable to open serial device %s', [Config.DeviceName]);
  result := serialhandle > 0;
end;

procedure TMainForm.RestoreConfigButtonClick(Sender: TObject);
begin
  Config.Load;
  UpdateGui;
end;

procedure TMainForm.SaveConfigButtonClick(Sender: TObject);
begin
  Config.Save;
end;

procedure TMainForm.SaveMacrosButtonClick(Sender: TObject);
begin
  with SaveDialog1 do begin
    filename := MacrosFileNameEdit.filename;
    if execute then begin
      filename := ChangeFileext(filename, '.macros');
      SaveMacros(filename);
      MacrosFileNameEdit.filename := filename;
      macrosmodified := false;
      UpdateGUI;
    end;
  end;
end;

procedure TMainForm.SaveMacrosQuitting(var Msg: TLMessage);
var
  mr: TModalResult;
  x, y: integer;
begin
  SaveMacrosButtonClick(nil);
  if macrosmodified then begin
    x := left + 100;
    y := top + 100;
    mr := MessageDlgPos('Close the application even if the modified macro definitions will be lost?',
      mtWarning, [mbYes, mbNo], 0, x, y);
    if (mr = mrNo) then
      exit;
    macrosmodified := false;
  end;
  close;
end;

procedure TMainForm.ShowKeysCheckBoxChange(Sender: TObject);
begin
  LayoutForm.visible := ShowKeysCheckBox.checked;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  k: integer;
  count: integer;
  sstate: TSerialState;
  inbuf: array[0..BUFSIZE-1] of char;
begin
  if serialhandle <= 0 then exit;
  count := SerRead(serialhandle, inbuf, sizeof(inbuf));
  if count = 0 then begin
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
      CloseSerialDevice(true);
      Log(llError, 'Serial connection with %s lost (LineState = %d)', [ Config.DeviceName, sstate.LineState]);
      Log(llError, 'Use the [Connect] button in Parameters to reconnect');
    end;
    {$ENDIF}
    exit;
  end;
  Callback(inbuf[0]);
  SerFlushInput(serialhandle);  // flush everything
end;

procedure TMainForm.UpdateGUI;
var
  i : integer;
begin
  for i := 0 to  BUTTON_COUNT-1 do
    MacrosEditor.Cells[1, i+1] := macros[i];
  DeviceEdit.Text := Config.DeviceName;
  BaudComboBox.Text := inttostr(Config.Baud);
  RadioButton1.checked := Config.logLevel = llDebug;
  RadioButton2.checked := Config.logLevel = llInfo;
  RadioButton3.checked := Config.logLevel = llError;
  RadioButton4.checked := Config.logLevel = llNone;
  TabSheet1.TabVisible := not RadioButton4.checked;
  SaveMacrosButton.Enabled := macrosmodified;
  DefaultCheckbox.checked := (Config.DefaultMacrosFile <> '')
    and (MacrosFileNameEdit.Filename = Config.DefaultMacrosFile);
  for i := 0 to  BUTTON_COUNT-1 do
    LayoutForm.keys[i].Hint := macros[i];
end;


end.

