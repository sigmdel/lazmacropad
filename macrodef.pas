unit macrodef;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, LCLIntf, LMessages, Buttons, Menus, ExtCtrls, StrUtils, params;

const
  LM_SAVE_MACROS_QUITTING = LM_USER + 1;

type

  { TMacroForm }

  TMacroForm = class(TForm)
    DefaultMacrosMenuItem: TMenuItem;
    InsertMacroMenuItem: TMenuItem;
    DeleteMacroMenuItem: TMenuItem;
    ClearMacrosMenuItem: TMenuItem;
    EraseMacroMenuItem: TMenuItem;
    ReloadMacrosFileMenuItem: TMenuItem;
    Separator2: TMenuItem;
    ModifiedLabel: TLabel;
    MacrosFilenameLabel: TLabel;
    OpenDialog1: TOpenDialog;
    EditorPopMenu: TPopupMenu;
    Separator1: TMenuItem;
    SaveMacroFileMenuItem: TMenuItem;
    OpenMacroFileMenuItem: TMenuItem;
    PopMacroMenuButton: TButton;
    MacrosLabel: TLabel;
    MacrosEditor: TStringGrid;
    MacrosPopupMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure ClearMacrosMenuItemClick(Sender: TObject);
    procedure DefaultMacrosMenuItemClick(Sender: TObject);
    procedure DeleteMacroMenuItemClick(Sender: TObject);
    procedure EditorPopMenuPopup(Sender: TObject);
    procedure EraseMacroMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure InsertMacroMenuItemClick(Sender: TObject);
    procedure MacrosEditorAfterSelection(Sender: TObject; aCol, aRow: Integer);
    procedure MacrosEditorEditingDone(Sender: TObject);
    procedure MacrosEditorSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OpenMacroFileMenuItemClick(Sender: TObject);
    procedure PopMacroMenuButtonClick(Sender: TObject);
    procedure ReloadMacrosFileMenuItemClick(Sender: TObject);
    procedure SaveMacroFileMenuItemClick(Sender: TObject);
  private
    function FixupMacroName(const fn: string): string;
    procedure UpdateGUI;
    procedure SetMacrosModified(value: boolean);
  public
    procedure SaveMacrosQuitting(var Msg: TLMessage); message LM_SAVE_MACROS_QUITTING;
    procedure SaveMacrosBeforeQuitting;
    procedure SetMacrosFilename(const value: string);
  end;

var
  MacroForm: TMacroForm;

resourcestring
  SpcCtrlV = 'Ctrl+V';                 // 'Ctrl+V'
  SpcShiftInsert = 'Shift+Insert';     // 'Shift+Insert'
  SpcCustom = 'Custom';                // 'Shift+Ctrl+Insert'
  SpcNone = 'None';                    // clipboards set but no paste command
  SpcKbdEvents = 'Kbd Events';         // keyboard up down events only, no clipboard


var
  sPasteCommands : array[TPasteCommand] of string =
    (SpcCtrlV, SpcShiftInsert, SpcCustom, SpcNone, SpcKbdEvents);

implementation

{$R *.lfm}

uses
  main, keymap, editmacro;

{ TMacroForm }

procedure TMacroForm.ClearMacrosMenuItemClick(Sender: TObject);
begin
  Config.Create;
  MacrosFilenameLabel.hint := '<new>';
  MacrosFilenameLabel.caption := MacrosFilenameLabel.hint;
  DefaultMacrosMenuItem.checked := false;
  SetMacrosModified(false);
  UpdateGUI;
end;

procedure TMacroForm.DefaultMacrosMenuItemClick(Sender: TObject);
var
  fn: string;
begin
  if DefaultMacrosMenuItem.checked then begin
     fn := MacrosFilenameLabel.hint;
     if (fn = '') or (fn = '<') then begin
        // never saved, do it now
        SaveMacroFileMenuItem.Click;
     end;
     fn := MacrosFilenameLabel.hint;
     if (fn = '') or (fn = '<') then begin
        // still not saved, don't show as default
        DefaultMacrosMenuItem.checked := false;
        exit;
     end;
      Config.DefaultMacrosFile := fn;
   end
   else if (MacrosFileNameLabel.hint = Config.DefaultMacrosFile) then
     DefaultMacrosMenuItem.checked := true;
end;

procedure TMacroForm.DeleteMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
  j: integer;
begin
  ndx := MacrosEditor.row - 1;
  for j := ndx to Config.ButtonCount-2 do begin
    Macros[j] := Macros[j+1];
    Pastes[j] := Pastes[j+1];
  end;
  Macros[Config.ButtonCount-1] := '';
  Pastes[Config.ButtonCount-1] := pcCtrlV;
  SetMacrosModified(true);
  UpdateGUI;
end;

procedure TMacroForm.EditorPopMenuPopup(Sender: TObject);
var
  ndx: integer;
begin
  ndx := MacrosEditor.row - 1;
  InsertMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount-1) and (Macros[ndx] <> '');
  EraseMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and (Macros[ndx] <> '');
  DeleteMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and (Macros[ndx] <> '');
end;

procedure TMacroForm.EraseMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
begin
  ndx := MacrosEditor.row - 1;
  if (ndx >= 0) and (ndx < Config.ButtonCount) and (Macros[ndx] <> '') then begin
    Macros[ndx] := '';
    Pastes[ndx] := pcCtrlV;
    SetMacrosModified(true);
    MacrosEditor.Cells[1, ndx+1] := '';
    MacrosEditor.Cells[2, ndx+1] := sPcCtrlV;
  end;
end;

function TMacroForm.FixupMacroName(const fn: string): string;
begin
  // assume fn = 'fname.macros.bak'
  result := ChangeFileext(fn, '');
   // avoid creating 'fname.macros.macros'
   while ExtractFileExt(result) = '.macros' do
     result := ChangeFileExt(result, '');
   result := ChangeFileext(result, '.macros');
end;

procedure TMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  MacrosEditor.RowCount := config.ButtonCount+1;
  for i := 1 to config.ButtonCount do
    MacrosEditor.Cells[0,i] := KeyLabels[i];
  UpdateGUI;
end;

procedure TMacroForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Width := (3*Width) div 2;
  Height := (3*Height) div 2;
end;

procedure TMacroForm.FormHide(Sender: TObject);
begin
  MainForm.MacroDefItem.Checked := false;
end;

procedure TMacroForm.InsertMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
  j: integer;
begin
  ndx := MacrosEditor.row - 1;
  for j := Config.ButtonCount-1 downto ndx+1 do begin
    Macros[j] := Macros[j-1];
    Pastes[j] := Pastes[j-1];
  end;
  Macros[ndx] := '';
  Pastes[ndx] := pcCtrlV;
  SetMacrosModified(true);
  UpdateGUI;
end;

procedure TMacroForm.MacrosEditorAfterSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  r, c: integer;
begin
  r := MacrosEditor.Row;
  c := MacrosEditor.Col;
  if c <> 1 then exit;
  if (r < 1) or (r > config.ButtonCount) then exit;
  dec(r);
  if pastes[r] <> pcKbdEvents then exit;
  EditKbdMacroForm.SetMacro(MacrosEditor.cells[c, r+1]);
  if EditKbdMacroForm.ShowModal = mrOk then
    MacrosEditor.cells[c, r+1] := EditKbdMacroForm.GetMacroString;
end;

procedure TMacroForm.MacrosEditorEditingDone(Sender: TObject);
var
  r, c: integer;
  s: string;
  pc: TPasteCommand;
begin
  r := MacrosEditor.Row;
  c := MacrosEditor.Col;
  s := MacrosEditor.Cells[c, r];
  if (r < 1) or (r > config.ButtonCount) then exit;
  dec(r);
  if MacrosEditor.Col = 1 then begin
    if s = macros[r] then exit;
    macros[r] := s;
    LayoutForm.keys[r].Hint := macros[r];
    SetMacrosModified(true);
    if s = '' then begin
      pastes[r] := pcCtrlV;
      MacrosEditor.Cells[2,r+1] := sPasteCommands[pcCtrlV];
    end;
  end
  else if MacrosEditor.Col = 2 then begin
    if s = sPasteCommands[pcCtrlV] then
      pc := pcCtrlV
    else if s = sPasteCommands[pcShiftInsert] then
      pc := pcShiftInsert
    else if s = sPasteCommands[pcCustom] then
      pc := pcCustom
    else if s = sPasteCommands[pcNone] then
      pc := pcNone
    else if s = sPasteCommands[pcKbdEvents] then
      pc := pcKbdEvents
    else
      exit;  ///
    if pastes[r] = pc then exit;
    pastes[r] := pc;
    SetMacrosModified(true);
  end;
end;

procedure TMacroForm.MacrosEditorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (aCol > 0) and (aRow > 0);
end;

procedure TMacroForm.OpenMacroFileMenuItemClick(Sender: TObject);
var
  fn: string;
begin
  with OpenDialog1 do begin
    filename := MacrosFilenameLabel.hint;
    if (filename = '') or (filename[1] = '<') then begin
      filename := '';
      initialdir := configdir;
    end;
    if execute then begin
      /// refactor this and  ReloadMacrosFileMenuItemClick(Sender: TObject);
      LoadMacros(filename);
      fn := FixupMacroName(filename);
      SetMacrosFilename(fn);
      SetMacrosModified(false);
      UpdateGUI;
      if (fn <> filename) and not FileExists(fn) then
        SaveMacros(fn);
    end;
  end;
end;

procedure TMacroForm.PopMacroMenuButtonClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := MacrosfilenameLabel.ClientOrigin;
  inc(pt.x, MacrosfilenameLabel.height+2);
  inc(pt.y, 20);
  MacrosPopupMenu.PopUp(pt.x, pt.y);
end;

procedure TMacroForm.ReloadMacrosFileMenuItemClick(Sender: TObject);
var
  fn: string;
  x, y: integer;
  mr: TModalResult;
begin
  fn := MacrosFilenameLabel.hint;
  if fileexists(fn) then begin
    if macrosmodified then begin
      x := left + 50;
      y := top + 50;
      mr := MessageDlgPos('Save the modified macro definitions before.',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0, x, y);
      if (mr = mrCancel) then exit;
      if (mr = mrYes) then
         SaveMacroFileMenuItemClick(nil);
    end;
    LoadMacros(fn);
    SetMacrosFilename(fn);
    SetMacrosModified(false);
    UpdateGUI;
  end
  else begin
    x := left + 50;
    y := top + 50;
    MessageDlgPos(Format('File ''%s'' does not exist.', [fn]),
      mtInformation, [mbOk], 0, x, y);
  end;
end;

procedure TMacroForm.SaveMacroFileMenuItemClick(Sender: TObject);
begin
  with SaveDialog1 do begin
    filename := MacrosFilenameLabel.hint;
    if (filename = '') or (filename[1] = '<') then begin
      filename := '';
      initialdir := configdir;
    end;
    if execute then begin
      // should not be necessary
      filename := FixupMacroName(filename);
      // assume 'fname.macros.bak'
      filename := ChangeFileext(filename, '');
      // avoid creating 'fname.macros.macros'
      while ExtractFileExt(filename) = '.macros' do
        ChangeFileExt(filename, '');
      filename := ChangeFileext(filename, '.macros');
      SaveMacros(filename);
      SetMacrosFilename(filename);
      SetMacrosModified(false);
      UpdateGUI;
    end;
  end;
end;

procedure TMacroForm.SaveMacrosBeforeQuitting;
begin
  PostMessage(self.handle, LM_SAVE_MACROS_QUITTING, 0, 0);
end;

procedure TMacroForm.SaveMacrosQuitting(var Msg: TLMessage);
var
  mr: TModalResult;
  x, y: integer;
begin
  SaveMacroFileMenuItemClick(nil);
  if macrosmodified then begin
    x := left + 100;
    y := top + 100;
    mr := MessageDlgPos('Close the application even if the modified macro definitions will be lost?',
      mtWarning, [mbYes, mbNo], 0, x, y);
    if (mr = mrNo) then
      exit;
    SetMacrosmodified(false);
  end;
  MainForm.close;
end;

procedure TMacroForm.SetMacrosFilename(const value: string);
begin
  MacrosFilenameLabel.hint := value;
  MacrosFilenameLabel.caption := ChangeFileExt(ExtractFileName(value), '');
  DefaultMacrosMenuItem.checked := (Config.DefaultMacrosFile <> '')
    and (value = Config.DefaultMacrosFile);
end;

procedure TMacroForm.SetMacrosModified(value: boolean);
begin
  macrosmodified := value;
  if value then
    DefaultMacrosMenuItem.checked := false;
  ModifiedLabel.Caption := ifThen(macrosmodified, '*', '');
end;

procedure TMacroForm.UpdateGUI;
var
  i : integer;
begin
  for i := 0 to  config.ButtonCount-1 do begin
    MacrosEditor.Cells[1, i+1] := macros[i];
    MacrosEditor.Cells[2, i+1] := sPasteCommands[pastes[i]];
  end;
  LayoutForm.UpdateGUI;
end;

end.

