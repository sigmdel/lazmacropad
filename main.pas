unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  MouseAndKeyInput, Clipbrd, LCLType;

type

  { TMainForm }
  TMainForm = class(TForm)
    CloseItem: TMenuItem;
    LogItem: TMenuItem;
    KeyLayoutItem: TMenuItem;
    MacroDefItem: TMenuItem;
    ShowWindowItem: TMenuItem;
    Separator1: TMenuItem;
    OptionsItem: TMenuItem;
    AboutItem: TMenuItem;
    Separator2: TMenuItem;
    Timer1: TTimer;
    TrayIconMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseItemClick(Sender: TObject);
    procedure LogItemClick(Sender: TObject);
    procedure KeyLayoutItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
    procedure MacroDefItemClick(Sender: TObject);
    procedure ShowWindowItemClick(Sender: TObject);
    procedure OptionsItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    FIconON: TIcon;
    FIconOFF: TIcon;
    procedure Callback(const src: string);
  public
    procedure SetTrayIcon(On: boolean);
    procedure Inject(index: integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  about, macrolog, macrodef, editstringmacro, options, keymap, serialreader, params, kbdev;

{ TMainForm }

procedure TMainForm.Callback(const src: string);
var
  i: integer;
begin
  if length(src) < 1 then begin
    LogForm.Log(llError, 'callback with empty message');
    exit;
  end;
  if (src[1] < ' ') then begin
    LogForm.log(llDebug, 'keypad syn message received');
    exit;
  end;
  i := KeyLabelToInt(src[1]);
  if (i < 0) or (i >= config.ButtonCount) then begin
    LogForm.log(llError, 'invalid keypad message "%s" received', [src]);
    exit;
  end;
  LogForm.log(llDebug, 'keypad message %d received', [i]);
  inject(i);
end;

procedure Delay(ms: QWORD);
var
  tc : QWORD;
begin
  tc := GetTickCount64 + ms;
  while (GetTickCount64 < tc) and (not Application.Terminated) do
    Application.ProcessMessages;
end;


procedure TMainForm.Inject(index: integer);
var
  convertedMacro: string;
  PasteCommand: TPasteCommand;
  WantsVK_RETURN: boolean;
  ndx: integer;
  macro: TKbdMacro;
  ShiftState: TShiftState;

  function KbdShiftToShiftState(kbdShift: TKbdShift): TShiftState;
  begin
    result := TShiftState(kbdShift * [ksShift, ksAlt, ksCtrl]);
    if ksAltGr in kbdShift then
      include(result, ssAltGr);
  end;

begin
  PasteCommand := pastes[index];
  LogForm.Log(llDebug, 'Inject macro %d with pasteCmd %s', [index, sPasteCommands[PasteCommand]]);

  if PasteCommand = pcKbdEvents then begin
     // no clipboard operations here
    macro := KbdMacros[index];
    if length(macro) < 1 then begin
      LogForm.Log(llInfo, 'Macro %d is not defined', [index]);
      exit;
    end;
    for ndx := 0 to length(macro)-1 do begin
      ShiftState := KbdShiftToShiftState(macro[ndx].Shift);
      if macro[ndx].Press then begin
        if ShiftState <> [] then begin
          LogForm.Log(llDebug,'keyboard event %d, applying shift keys %s', [ndx, macro[ndx].ShiftStateToStr]);
          KeyInput.Apply(ShiftState);
        end;
        LogForm.Log(llDebug,'keyboard event %d, key: %x down', [ndx, macro[ndx].code]);
        KeyInput.Down(macro[ndx].Code);
      end
      else begin
        LogForm.Log(llDebug,'keyboard event %d, key: %x up', [ndx, macro[ndx].code]);
        KeyInput.Up(macro[ndx].code);
        if ShiftState <> [] then begin
          LogForm.Log(llDebug,'keyboard event %d, unapplying shift keys %s', [ndx, macro[ndx].ShiftStateToStr]);
          KeyInput.UnApply(ShiftState);
        end;
      end;
    end;
    LogForm.Log(llInfo,'%d keyboard events injected', [length(macro)]);
    exit;
  end;

  convertedMacro := RemoveEscSequences(StringMacros[index]);
  if convertedMacro = '' then begin
    LogForm.Log(llInfo, 'Macro %d is not defined', [index]);
    exit;
  end;
  if PasteCommand <> pcNone then begin
    WantsVK_RETURN := convertedMacro[length(ConvertedMacro)] = #13;
    if WantsVK_RETURN then
      setlength(convertedMacro, length(convertedMacro)-1);
  end;
  clipboard.AsText := convertedMacro;
  PrimarySelection.Astext := convertedMacro; // always sychronize

  if PasteCommand <= pcCustom then with pasteCommands[ord(PasteCommand)] do begin
    ShiftState := KbdShiftToShiftState(Shift);
    Delay(Delayms);
    KeyInput.Apply(ShiftState);
    KeyInput.Press(Code);
    KeyInput.Unapply(ShiftState);
    LogForm.Log(llDebug,'Paste with %s', [sPasteCommands[PasteCommand]]);
    if WantsVK_RETURN then begin
       // A relatively long delay before injecting the VK_RETURN keyboard event
       // may be needed. Otherwise, the clipboard paste operation will not
       // be completed and only the return key will be posted. This problem
       // has been observed to occur in VSCodium with the QT5 widget set.
       Delay(VkReturnDelay);
       KeyInput.Press(VK_RETURN);
       LogForm.Log(llDebug,'Appended VK_RETURN');
    end;
  end;
end;


{ #todo 3 -oMichel -cAppearance : Fix position of dialogues in current screen }

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
    if (mr = mrYes) then
      mr := MacroForm.SaveMacrosFile;
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
begin
  FIconON := TIcon.Create;
  FIconON.LoadFromResourceName(Hinstance,  'LAZMACROPAD_ON');
  FIconOFF := TIcon.Create;
  FIconOFF.LoadFromResourceName(Hinstance,  'LAZMACROPAD_OFF');
  //SetTrayIcon(false); // don't do this to get a fighting chance at showing the correct icon!
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  hide;  // must do that here and not in FormShow or else
         // the clipboard will not work in GTK2
  // https://forum.lazarus.freepascal.org/index.php/topic,61044.0.html
  // [SOLVED] Non functioning clipboard in Linux TrayIcon  (Read 27 times)
  paramsInit;
  // need config.KeyCols and config.KeyRows to create the
  // key map
  Application.CreateForm(TLayoutForm, LayoutForm);

  if Config.loglevel >= llError then
    LogForm.log(Config.loglevel, 'Change the log level in parameters to see more information.');
  MacroForm.SetMacrosFilename(Config.DefaultMacrosFile);
  MacroForm.SaveDialog1.InitialDir := configdir;
  Timer1.Enabled := OpenSerial;
  SetTrayIcon(Timer1.Enabled);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FIconOFF.free;
  FIconON.free;
end;

procedure TMainForm.SetTrayIcon(On: boolean);
begin
  if On then
    TrayIcon.Icon.Assign(FIconON)
  else
    TrayIcon.Icon.Assign(FIconOFF);
  TrayIcon.InternalUpdate;  // "always a safe function to call, regardless of the widgetset."
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  rxc: char;
begin
  rxc := ReadSerial;
  if rxc > #0 then
    Callback(rxc);
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  TrayIcon.PopUpMenu.PopUp;
end;


// <tray menu>

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  AboutForm.ShowModal
end;

procedure TMainForm.CloseItemClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.KeyLayoutItemClick(Sender: TObject);
begin
  LayoutForm.visible := not LayoutForm.visible;
end;

procedure TMainForm.LogItemClick(Sender: TObject);
begin
  LogForm.visible := not LogForm.visible;
end;

procedure TMainForm.MacroDefItemClick(Sender: TObject);
begin
  MacroForm.visible := not MacroForm.visible;
end;

procedure TMainForm.OptionsItemClick(Sender: TObject);
begin
  OptionsForm.visible := not OptionsForm.visible;
end;

procedure TMainForm.ShowWindowItemClick(Sender: TObject);
begin
  if KeyLayoutItem.Checked then
    LayoutForm.BringToFront;
  if MacroDefItem.Checked then
    MacroForm.BringToFront;
  if OptionsItem.Checked then
    OptionsForm.BringToFront;
  if LogItem.Checked then
    LogForm.BringToFront;
end;

// </tray menu>

end.

