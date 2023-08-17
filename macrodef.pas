unit macrodef;

{$mode ObjFPC}{$H+}

{ #todo 1 -oMichel -cBug : Clean up handling of macros file such as loading a file over current modified definitions without checking!}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, LCLIntf, Buttons, Menus, ExtCtrls, StrUtils, params;

type

  { TMacroForm }

  TMacroForm = class(TForm)
    CustomPasteMenuItem: TMenuItem;
    EditMacroMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    Separator4: TMenuItem;
    SaveMacroFileMenuItem: TMenuItem;
    ProxyEditorButton: TButton;
    DefaultMacrosMenuItem: TMenuItem;
    InsertMacroMenuItem: TMenuItem;
    DeleteMacroMenuItem: TMenuItem;
    ClearMacrosMenuItem: TMenuItem;
    EraseMacroMenuItem: TMenuItem;
    MoveMacroUpMenuItem: TMenuItem;
    MoveMacroDownMenuItem: TMenuItem;
    Separator3: TMenuItem;
    ReloadMacrosFileMenuItem: TMenuItem;
    Separator2: TMenuItem;
    ModifiedLabel: TLabel;
    MacrosFilenameLabel: TLabel;
    OpenDialog1: TOpenDialog;
    EditorPopMenu: TPopupMenu;
    Separator1: TMenuItem;
    SaveMacroFileAsMenuItem: TMenuItem;
    OpenMacroFileMenuItem: TMenuItem;
    PopMacroMenuButton: TButton;
    MacrosLabel: TLabel;
    MacrosEditor: TStringGrid;
    MacrosPopupMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure EditMacroMenuItemClick(Sender: TObject);
    procedure MacrosEditorSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure CustomPasteMenuItemClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MoveMacroDownMenuItemClick(Sender: TObject);
    procedure MoveMacroUpMenuItemClick(Sender: TObject);
    procedure ProxyEditorButtonEnter(Sender: TObject);
    procedure ClearMacrosMenuItemClick(Sender: TObject);
    procedure DefaultMacrosMenuItemClick(Sender: TObject);
    procedure DeleteMacroMenuItemClick(Sender: TObject);
    procedure EditorPopMenuPopup(Sender: TObject);
    procedure EraseMacroMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure InsertMacroMenuItemClick(Sender: TObject);
    procedure MacrosEditorEditingDone(Sender: TObject);
    procedure MacrosEditorSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure OpenMacroFileMenuItemClick(Sender: TObject);
    procedure PopMacroMenuButtonClick(Sender: TObject);
    procedure ReloadMacrosFileMenuItemClick(Sender: TObject);
    procedure SaveMacroFileAsMenuItemClick(Sender: TObject);
    procedure SaveMacroFileMenuItemClick(Sender: TObject);
  private
    function FixupMacroName(const fn: string): string;
    procedure UpdateGUI;
    procedure ProxyEditorRestoreSelection(Data: PtrInt);
  public
      // returns mrOk if saved, or not saved with user confirmation
      // returns mrCancel if user canceled save operation
    function SaveMacrosFile: integer;
    procedure SetMacrosFilename(const value: string);
    procedure SetMacrosModified(value: boolean);
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
  main, keymap, kbdev, macrolog, editstringmacro, editkbdmacro,
  custompastecommand, editpastedelays;

{ TMacroForm }

procedure TMacroForm.ClearMacrosMenuItemClick(Sender: TObject);
begin
  Config.ClearMacros;
  MacrosFilenameLabel.hint := '<new>';
  MacrosFilenameLabel.caption := MacrosFilenameLabel.hint;
  DefaultMacrosMenuItem.checked := false;
  SetMacrosModified(false);
  UpdateGUI;
  LogForm.Log(llDebug, 'Cleared all macros');
end;

procedure TMacroForm.CustomPasteMenuItemClick(Sender: TObject);
var
  ek: TKbdEvent;
begin
  ek := PasteCommands[ord(pcCustom)];
  if EditCustomPasteCommand(ek) and (ek <> PasteCommands[ord(pcCustom)]) then begin
    PasteCommands[ord(pcCustom)] := ek;
    SetMacrosModified(true);
  end;
end;

procedure TMacroForm.MenuItem1Click(Sender: TObject);
begin
  if doEditPasteDelays then
    SetMacrosModified(true);
end;

procedure TMacroForm.DefaultMacrosMenuItemClick(Sender: TObject);
var
  fn: string;
begin
  if DefaultMacrosMenuItem.checked then begin
     // user wants to set current file as default macro file
     fn := MacrosFilenameLabel.hint;
     if (fn = '') or (fn = '<') then begin
        // never saved, do it now, could result in new filename
        SaveMacroFileAsMenuItem.Click;
     end;
     fn := MacrosFilenameLabel.hint;
     if (fn = '') or (fn = '<') then begin
        // still not saved, don't show as default
        DefaultMacrosMenuItem.checked := false;
        LogForm.Log(llDebug, 'Invalid filename ("%s") for a default macro file', [fn]);
        exit;
     end;
     Config.DefaultMacrosFile := fn;
     LogForm.Log(llDebug, 'File "%s" set as default macro file', [fn]);
   end
   else begin
     // user does not want current file as default macro file
     if (MacrosFileNameLabel.hint = Config.DefaultMacrosFile) then begin
       DefaultMacrosMenuItem.checked := true;
       LogForm.Log(llDebug, 'File "%s" remains default macro file', [Config.DefaultMacrosFile]);
     end;
   end;
end;

procedure TMacroForm.DeleteMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
  j: integer;
begin
  ndx := MacrosEditor.row - 1;
  for j := ndx to Config.ButtonCount-2 do begin
    StringMacros[j] := StringMacros[j+1];
    KbdMacros[j] := KbdMacros[j+1];
    Pastes[j] := Pastes[j+1];
  end;
  StringMacros[Config.ButtonCount-1] := '';
  SetLength(KbdMacros[Config.ButtonCount-1], 0);
  Pastes[Config.ButtonCount-1] := pcCtrlV;
  SetMacrosModified(true);
  UpdateGUI;
  LogForm.Log(llDebug, 'Deleted macro %d', [ndx]);
end;

procedure TMacroForm.EditMacroMenuItemClick(Sender: TObject);
var
  r, c: integer;
begin
  c := MacrosEditor.col;
  if c <> 1 then exit;
  r := MacrosEditor.row;
  if (r < 1) or (r > Config.ButtonCount) then exit;
  if Pastes[r-1] = pcKbdEvents then
    ProxyEditorButtonEnter(Sender)
  else begin
    EditStringForm.SetEditText(MacrosEditor.Cells[c, r]);
    if EditStringForm.ShowModal = mrOk then
      MacrosEditor.Cells[c, r] := EditStringForm.GetEditText;
  end;
end;

procedure TMacroForm.EditorPopMenuPopup(Sender: TObject);

  function MacrosEmptyFrom(index: integer): boolean;
  begin
    result := false;
    repeat
      if (StringMacros[index] <> '') or (length(KbdMacros) > 0) then
        exit;
      inc(index);
    until (index >= Config.ButtonCount);
    result := true;
  end;

var
  ndx: integer;
  notEmpty: boolean;
begin
  //if MacrosEditor.Col <> 1 then
    //TPopUpMenu(Sender).Free;
  ndx := MacrosEditor.row - 1;
  notEmpty := not MacrosEmptyFrom(ndx);
  InsertMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount-1) and notEmpty;
  EraseMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and (StringMacros[ndx] <> '');
  DeleteMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and notEmpty;
  MoveMacroUpMenuItem.Enabled := (ndx > 0);
  MoveMacroDownMenuItem.Enabled := (ndx < Config.ButtonCount-1);
  ClearMacrosMenuItem.Enabled := not MacrosEmptyFrom(0);
  LogForm.Log(llDebug, 'Macros editor context menu popup');
end;

procedure TMacroForm.EraseMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
begin
  ndx := MacrosEditor.row - 1;
  if (ndx >= 0) and (ndx < Config.ButtonCount) and ((StringMacros[ndx] <> '') or (length(KbdMacros[ndx]) > 0)) then begin
    StringMacros[ndx] := '';
    SetLength(KbdMacros[ndx], 0);
    Pastes[ndx] := pcCtrlV;
    SetMacrosModified(true);
    MacrosEditor.Cells[1, ndx+1] := '';
    MacrosEditor.Cells[2, ndx+1] := sPcCtrlV;
    LayoutForm.keys[ndx].Hint := '';
  end;
  LogForm.Log(llDebug, 'Erased macro %d, keymap form updated', [ndx]);
end;

function TMacroForm.FixupMacroName(const fn: string): string;
begin
  // assume fn = 'fname.macros.bak'
  result := ChangeFileext(fn, '');
   // avoid creating 'fname.macros.macros'
   while ExtractFileExt(result) = '.macros' do
     result := ChangeFileExt(result, '');
   result := ChangeFileext(result, '.macros');
   if result <> fn then
     LogForm.Log(llDebug, 'Filename %s changed to %s', [fn, result]);
end;

procedure TMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  MacrosEditor.RowCount := config.ButtonCount+1;
  for i := 1 to config.ButtonCount do
    MacrosEditor.Cells[0,i] := KeyLabels[i];
  UpdateGUI;
  LogForm.Log(llDebug, 'Macros editor form activated');
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
  LogForm.Log(llDebug, 'Macro editor form hide; unchecked Macro Definitions menu item in main form');
end;

{ #todo 1 -oMichel -cImprovement : Verify before if deleting last macro is ok }
procedure TMacroForm.InsertMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
  j: integer;
begin
  ndx := MacrosEditor.row - 1;
  for j := Config.ButtonCount-1 downto ndx+1 do begin
    StringMacros[j] := StringMacros[j-1];
    KbdMacros[j] := KbdMacros[j-1];
    Pastes[j] := Pastes[j-1];
  end;
  StringMacros[ndx] := '';
  SetLength(KbdMacros[ndx], 0);
  Pastes[ndx] := pcCtrlV;
  SetMacrosModified(true);
  UpdateGUI;
  LogForm.Log(llDebug, 'Inserted an empty macro at index %d', [ndx]);
end;

procedure TMacroForm.MacrosEditorEditingDone(Sender: TObject);
// not executed when editing kbdmacro
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
  if c = 1 then begin
    if s = StringMacros[r] then exit;
    StringMacros[r] := s;
    LayoutForm.keys[r].Hint := StringMacros[r];
    SetMacrosModified(true);
    if s = '' then begin
      pastes[r] := pcCtrlV;
      MacrosEditor.Cells[2,r+1] := sPasteCommands[pcCtrlV];
    end;
    LogForm.Log(llDebug, 'Macro %d modified, layout form updated', [r]);
  end
  else if c = 2 then begin
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
    else begin
      MacrosEditor.Cells[c, r+1] := sPasteCommands[pastes[r]];
      exit;
    end;
    if pastes[r] = pc then exit;
    pastes[r] := pc;
    SetMacrosModified(true);
    LogForm.Log(llDebug, 'Paste command %d modified', [r]);
  end;
end;

procedure TMacroForm.MacrosEditorSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if (aCol = 1) and (Pastes[ARow-1] = pcKbdEvents) then begin
    ProxyEditorButton.BoundsRect := Rect(10, 10, 8, 8); // make it invisible
    Editor := ProxyEditorButton;
    LogForm.Log(llDebug, 'Proxy editor set for cell[%d, %d]', [aCol, aRow]);
  end;
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
      { #todo -cRefactor : refactor this and  ReloadMacrosFileMenuItemClick(Sender: TObject); }
      LoadMacros(filename);
      LogForm.Log(llDebug, 'Loaded macro file "%s"', [filename]);
      fn := FixupMacroName(filename);
      SetMacrosFilename(fn);
      SetMacrosModified(false);
      UpdateGUI;
      if (fn <> filename) and not FileExists(fn) then begin
        SaveMacros(fn);
        LogForm.Log(llDebug, 'Saved macro file as "%s"', [fn]);
      end;
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

procedure TMacroForm.ProxyEditorButtonEnter(Sender: TObject);
var
  r, c: integer;
begin
  r := MacrosEditor.Row;
  c := MacrosEditor.Col;
  if c <> 1 then exit;
  if (r < 1) or (r > config.ButtonCount) then exit;
  EditKbdMacroForm.SetMacro(KbdMacros[r-1]); // = cells[c, r]
  LogForm.Log(llDebug, 'Starting Keyboard macro editor');
  if EditKbdMacroForm.ShowModal = mrOk then begin
    setlength(KbdMacros[r-1], 0);
    KbdMacros[r-1] := EditKbdMacroForm.GetKbdMacro;
    StringMacros[r-1] := EditKbdMacroForm.GetMacroString;
    LayoutForm.keys[r-1].Hint := EditKbdMacroForm.GetMacroString;;
    if MacrosEditor.cells[c, r] <> EditKbdMacroForm.GetMacroString then begin
      MacrosEditor.cells[c, r] := EditKbdMacroForm.GetMacroString;
      SetMacrosModified(true);
      LogForm.Log(llDebug, 'Macro %d set to %s', [r-1, EditKbdMacroForm.GetMacroString]);
    end;
  end;
  Application.QueueAsyncCall(@ProxyEditorRestoreSelection, 0);
end;

procedure TMacroForm.MoveMacroUpMenuItemClick(Sender: TObject);
var
  tempmac: string;
  tempkbdmac: TKbdMacro;
  temppc: TPasteCommand;
  r: integer;
begin
  r := MacrosEditor.Row;
  if r <= 0 then exit;
  dec(r);
  tempmac := StringMacros[r-1];
  tempkbdmac := KbdMacros[r-1];
  temppc := pastes[r-1];

  StringMacros[r-1] := StringMacros[r];
  KbdMacros[r-1] := KbdMacros[r];
  pastes[r-1] := pastes[r];

  StringMacros[r] := tempmac;
  KbdMacros[r] := tempkbdmac;
  pastes[r] := temppc;

  UpdateGUI;
  MacrosEditor.Row := r;
  SetMacrosModified(true);
  LogForm.Log(llDebug, 'Moved macro %d to %d', [r, r-1]);
end;

procedure TMacroForm.MoveMacroDownMenuItemClick(Sender: TObject);
var
  tempmac: string;
  tempkbdmac: TKbdMacro;
  temppc: TPasteCommand;
  r: integer;
begin
  r := MacrosEditor.Row;
  dec(r);
  if r >= Config.ButtonCount-1 then exit;

  tempmac := StringMacros[r];
  tempkbdmac := KbdMacros[r];
  temppc := pastes[r];

  StringMacros[r] := StringMacros[r+1];
  KbdMacros[r] := KbdMacros[r+1];
  pastes[r] := pastes[r+1];

  StringMacros[r+1] := tempmac;
  KbdMacros[r+1] := tempkbdmac;
  pastes[r+1] := temppc;

  UpdateGUI;
  MacrosEditor.Row := r+2;
  SetMacrosModified(true);
  LogForm.Log(llDebug, 'Moved macro %d to %d', [r, r+1]);
end;

procedure TMacroForm.MacrosEditorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := aCol > 0;
  EditorPopMenu.AutoPopup := aCol = 1;
end;

procedure TMacroForm.ProxyEditorRestoreSelection(Data: PtrInt);
begin
  // MacrosEditor.Col := -1; doesn't seem to work in Windows
  MacrosEditor.Col := 2;
  MacrosEditor.Col := 1;
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
    LogForm.Log(llDebug, 'Reloaded macro file "%s"', [fn]);
  end
  else begin
    x := left + 50;
    y := top + 50;
    MessageDlgPos(Format('File ''%s'' does not exist.', [fn]),
      mtInformation, [mbOk], 0, x, y);
  end;
end;

procedure TMacroForm.SaveMacroFileAsMenuItemClick(Sender: TObject);
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
      LogForm.Log(llDebug, 'Save macros to file "%s"', [filename]);
    end;
  end;
end;

procedure TMacroForm.SaveMacroFileMenuItemClick(Sender: TObject);
var
  fn: string;
begin
  fn := MacrosFilenameLabel.hint;
  if (fn = '') or (fn[1] = '<') then
    SaveMacroFileAsMenuItemClick(Sender)
  else begin
    SaveMacros(fn);
    SetMacrosModified(false);
  end;
end;

function TMacroForm.SaveMacrosFile: integer;
var
  mr: TModalResult;
  x, y: integer;
begin
  result := mrOk;
  SaveMacroFileMenuItemClick(nil);
  if macrosmodified then begin
    // file not saved
    x := left + 100;
    y := top + 100;
    mr := MessageDlgPos('Close the application even if the modified macro definitions will be lost?',
      mtWarning, [mbYes, mbNo], 0, x, y);
    if (mr = mrNo) then
      result := mrCancel;
  end;
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
  LogForm.Log(llDebug, 'macrosmodified set to %s', [ifThen(value, 'True', 'False')]);
end;

procedure TMacroForm.UpdateGUI;
var
  i : integer;
begin
  for i := 0 to  config.ButtonCount-1 do begin
    MacrosEditor.Cells[2, i+1] := sPasteCommands[pastes[i]];
    if pastes[i] = pcKbdEvents then
      MacrosEditor.Cells[1, i+1] := KbdMacroToStr(KbdMacros[i])
    else
      MacrosEditor.Cells[1, i+1] := StringMacros[i];
  end;
  LayoutForm.UpdateGUI;
  LogForm.Log(llDebug, 'Updated Macro editor and keymap forms');
end;

end.

