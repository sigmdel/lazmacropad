unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  MouseAndKeyInput, Clipbrd, StrUtils, LCLType;

type

  { TMainForm }
  TMainForm = class(TForm)
    CloseItem: TMenuItem;
    LogItem: TMenuItem;
    KeyLayoutItem: TMenuItem;
    MacroDefItem: TMenuItem;
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
    procedure OptionsItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
  about, macrolog, macrodef, options, keymap, serialreader, params;

function convertEscSequences(const ins: string): string;
begin
  result := replaceStr(ins, '\n', #13);
  result := replaceStr(result, '\t', #9);
  result := replacestr(result, '\\', '\');
end;

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
  if (macros[i] <> '') then begin
    inject(i);
    LogForm.Log(llInfo, 'Key: %s, Macro: %s', [src, macros[i]]);
  end
  else
    LogForm.Log(llInfo, 'Key: %s, no macro defined', [src]);
end;

procedure TMainForm.Inject(index: integer);
var
  convertedMacro: string;
  PasteCommand: TPasteCommand;
begin
  convertedMacro := convertEscSequences(macros[index]);
  if convertedMacro = '' then exit;

  clipboard.AsText := convertedMacro;
  PasteCommand := pastes[index];
  {$ifndef WINDOWS}
  if PasteCommand = pcShiftInsert then begin
    PrimarySelection.Astext := convertedMacro;
    KeyInput.Apply([ssShift]);
    KeyInput.Press(VK_INSERT);
    KeyInput.Unapply([ssShift]);
    LogForm.Log(llDebug,'Paste with Shift+Insert');
  end
  else begin
  {$endif}
    KeyInput.Apply([ssCtrl]);
    KeyInput.Press(VK_V);
    KeyInput.Unapply([ssCtrl]);
    LogForm.Log(llDebug,'Paste with Ctrl+V');
  {$ifndef WINDOWS}
  end;
  {$endif}
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
  SetTrayIcon(false);
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

  Timer1.Enabled := OpenSerial;

  if Config.loglevel >= llError then
    LogForm.log(Config.loglevel, 'Change the log level in parameters to see more information.');
  MacroForm.MacrosFileNameEdit.Filename := Config.DefaultMacrosFile;
  MacroForm.SaveDialog1.InitialDir := configdir;
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
  repeat
    application.processmessages;
  until TrayIcon.Show;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  rxc: char;
begin
  rxc := ReadSerial;
  if rxc > #0 then
    Callback(rxc);
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

// </tray menu>

end.

