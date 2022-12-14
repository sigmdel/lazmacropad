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
  LogForm.Log(llDebug, 'callback(%s)', [src]);
  if length(src) < 1 then exit;
  i := KeyLabelToInt(src[1]);
  if (i < 0) or (i >= config.ButtonCount) then
    exit;
  inject(i);
end;

{$DEFINE VK_RETURN_SPECIAL}

/// Temporarily remove Windows considerations
/// Over logged for now!
///
procedure TMainForm.Inject(index: integer);
var
  convertedMacro: string;
  PasteCommand: TPasteCommand;
  {$ifdef VK_RETURN_SPECIAL}
  WantsVK_RETURN: boolean;
  {$endif}
  ndx: integer;
  macro: TKbdMacro;
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
      if macro[ndx].Press then begin
        if macro[ndx].Shift <> [] then begin
          LogForm.Log(llDebug,'keyboard event %d, applying shift keys %x', [ndx, integer(macro[ndx].Shift)]);
          KeyInput.Apply(macro[ndx].Shift);
        end;
        LogForm.Log(llDebug,'keyboard event %d, key: %x down', [ndx, macro[ndx].VK]);
        KeyInput.Down(macro[ndx].VK);
      end
      else begin
        LogForm.Log(llDebug,'keyboard event %d, key: %x up', [ndx, macro[ndx].VK]);
        KeyInput.Up(macro[ndx].VK);
        if macro[ndx].Shift <> [] then begin
          LogForm.Log(llDebug,'keyboard event %d, unapplying shift keys %x', [ndx, integer(macro[ndx].Shift)]);
          KeyInput.UnApply(macro[ndx].Shift);
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
  {$ifdef VK_RETURN_SPECIAL}
  if PasteCommand <> pcNone then begin
    WantsVK_RETURN := convertedMacro[length(ConvertedMacro)] = #13;
    if WantsVK_RETURN then
      setlength(convertedMacro, length(convertedMacro)-1);
  end;
  {$endif}
  clipboard.AsText := convertedMacro;
  PrimarySelection.Astext := convertedMacro; // always sychronize

  if PasteCommand = pcShiftInsert then begin
    KeyInput.Apply([ssShift]);
    KeyInput.Press(VK_INSERT);
    KeyInput.Unapply([ssShift]);
    {$ifdef VK_RETURN_SPECIAL}
    if WantsVK_RETURN then
      KeyInput.Press(VK_RETURN);
    {$endif}
    LogForm.Log(llDebug,'Paste with Shift+Insert');
  end
  else if PasteCommand = pcCustom then begin
    KeyInput.Apply(CustomPaste.Shift);
    KeyInput.Press(CustomPaste.VK);
    KeyInput.Unapply(CustomPaste.Shift);
    {$ifdef VK_RETURN_SPECIAL}
    if WantsVK_RETURN then
      KeyInput.Press(VK_RETURN);
    {$endif}
    //LogForm.Log(llDebug,'Paste with custom paste command');
  end
  else if PasteCommand = pcCtrlV then begin
    KeyInput.Apply([ssCtrl]);
    KeyInput.Press(VK_V);
    KeyInput.Unapply([ssCtrl]);
    {$ifdef VK_RETURN_SPECIAL}
    if WantsVK_RETURN then
      KeyInput.Press(VK_RETURN);
    {$endif}
    LogForm.Log(llDebug,'Paste with Ctrl+V');
  end
  //else PasteCommand = pcNone
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
    if (mr = mrYes) then begin
      MacroForm.SaveMacrosBeforeQuitting;
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

