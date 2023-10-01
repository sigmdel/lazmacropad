unit main;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

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
  about, macrodef, options, keymap, macrolog, editstringmacro,
  userial, ukbdev, umacros, uconfig, ulog;

{ TMainForm }

procedure TMainForm.Callback(const src: string);
const
  _log_name_ = 'TMainForm.Callback';
var
  i: integer;
begin
  if length(src) < 1 then begin
    LogOut(llError, _log_name_+': '+'Param src is empty');
    exit;
  end;
  if (src[1] < ' ') then begin
    LogOut(llDebug, _log_name_+': '+'Keypad syn message received');
    exit;
  end;
  i := KeyLabelToInt(src[1]);
  if (i < 0) or (i >= config.ButtonCount) then begin
    LogOut(llError, _log_name_+': '+'Invalid keypad message "%s" received', [src]);
    exit;
  end;
  LogOut(llDebug, _log_name_+': '+'Keypad message %d received', [i]);
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
const
  _log_name_ = 'TMainForm.Inject';
var
  convertedMacro: string;
  PasteCommand: TMacroKind;
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
  PasteCommand := macros[index].Kind;
  LogOut(llDebug, _log_name_+': '+'Macro %d', [index]);
  if macros[index].isEmpty then begin
    LogOut(llInfo, _log_name_+': '+'Macro %d is not defined', [index]);
    exit;
  end;

  if PasteCommand = mkKbdMacro then begin
    macro := macros[index].events;
    for ndx := 0 to length(macro)-1 do begin
      ShiftState := KbdShiftToShiftState(macro[ndx].Shift);
      if macro[ndx].Press then begin
        if ShiftState <> [] then begin
          LogOut(llDebug, _log_name_+': '+'Keyboard event %d, applying shift keys %s', [ndx, macro[ndx].ShiftStateToStr]);
          KeyInput.Apply(ShiftState);
        end;
        LogOut(llDebug,_log_name_+': '+'Keyboard event %d, key: %x down', [ndx, macro[ndx].code]);
        KeyInput.Down(macro[ndx].Code);
      end
      else begin
        LogOut(llDebug, _log_name_+': '+'Keyboard event %d, key: %x up', [ndx, macro[ndx].code]);
        KeyInput.Up(macro[ndx].code);
        if ShiftState <> [] then begin
          LogOut(llDebug,_log_name_+': '+'Keyboard event %d, unapplying shift keys %s', [ndx, macro[ndx].ShiftStateToStr]);
          KeyInput.UnApply(ShiftState);
        end;
      end;
    end;
    LogOut(llInfo,_log_name_+': '+'KbdMacro %d with %d keyboard events injected', [index, length(macro)]);
    exit;
  end;

  convertedMacro := RemoveEscSequences(macros[index].content);
  if PasteCommand <> mkNone then begin
    WantsVK_RETURN := convertedMacro[length(ConvertedMacro)] = #13;
    if WantsVK_RETURN then
      setlength(convertedMacro, length(convertedMacro)-1);
  end;
  clipboard.AsText := convertedMacro;
  PrimarySelection.Astext := convertedMacro; // always sychronize

  if PasteCommand <= pcCustom then with macros.PasteShortcut[PasteCommand] do begin
    ShiftState := KbdShiftToShiftState(Shift);
    Delay(Delayms);
    KeyInput.Apply(ShiftState);
    KeyInput.Press(Code);
    KeyInput.Unapply(ShiftState);
    LogOut(llDebug, _log_name_+': '+'String macro %d pasted with %s', [index, sMacroKind[PasteCommand]]);
    if WantsVK_RETURN then begin
       // A relatively long delay before injecting the VK_RETURN keyboard event
       // may be needed. Otherwise, the clipboard paste operation will not
       // be completed and only the return key will be posted. This problem
       // has been observed to occur in VSCodium with the QT5 widget set.
       Delay(macros.ReturnDelay);
       KeyInput.Press(VK_RETURN);
       LogOut(llDebug, _log_name_+': '+'Appended VK_RETURN');
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
  if macros.modified then begin
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
  Width := 4;
  Height := 4;
  FIconON := TIcon.Create;
  FIconON.LoadFromResourceName(Hinstance,  'LAZMACROPAD_ON');
  FIconOFF := TIcon.Create;
  FIconOFF.LoadFromResourceName(Hinstance,  'LAZMACROPAD_OFF');
  //SetTrayIcon(false); // don't do this to get a fighting chance at showing the correct icon!
  TrayIcon.Icon.Assign(FIconOFF);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  hide;  // must do that here and not in FormShow or else
         // the clipboard will not work in GTK2
  // https://forum.lazarus.freepascal.org/index.php/topic,61044.0.html
  // [SOLVED] Non functioning clipboard in Linux TrayIcon  (Read 27 times)
  //LoadConfig;
  // need config.KeyCols and config.KeyRows to create the
  // key map
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


