unit macrodef;

{$mode ObjFPC}{$H+}

{ #todo 1 -oMichel -cBug : Clean up handling of macros file such as loading a file over current modified definitions without checking!}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, LCLIntf, Buttons, Menus, ExtCtrls, StrUtils;

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
    guiUpdated: boolean;
    function FixupMacroName(const fn: string): string;
    procedure UpdateGUI;
    procedure ProxyEditorRestoreSelection(Data: PtrInt);
  public
      // returns mrOk if saved, or not saved with user confirmation
      // returns mrCancel if user canceled save operation
    function SaveMacrosFile: integer;
    procedure SetMacrosFilename(const value: string);
    procedure MacrosModified(Sender: TObject);
  end;

var
  MacroForm: TMacroForm;

implementation

{$R *.lfm}

uses
  main, keymap, ulog, uconfig, ukbdev, umacros, editstringmacro, editkbdmacro,
  custompastecommand, editpastedelays;

{ TMacroForm }

procedure TMacroForm.ClearMacrosMenuItemClick(Sender: TObject);
begin
  macros.Clear;
  MacrosFilenameLabel.hint := '<new>';
  MacrosFilenameLabel.caption := MacrosFilenameLabel.hint;
  DefaultMacrosMenuItem.checked := false;
  macros.Modified := false;
  UpdateGUI;
end;

procedure TMacroForm.CustomPasteMenuItemClick(Sender: TObject);
var
  ek: TKbdEvent;
begin
  ek := macros.CustomShortcut;
  if EditCustomPasteCommand(ek) then
    macros.CustomShortcut := ek;
end;

procedure TMacroForm.DefaultMacrosMenuItemClick(Sender: TObject);
const
  _log_name_ = 'TMacro.setEvents';
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
        LogOut(llError, _log_name_+': '+'Invalid filename ("%s") for a default macro file', [fn]);
        exit;
     end;
     Config.DefaultMacrosFile := fn;
     LogOut(llDebug, _log_name_+': '+'File "%s" set as default macro file', [fn]);
   end
   else begin
     // user does not want current file as default macro file
     if (MacrosFileNameLabel.hint = Config.DefaultMacrosFile) then begin
       DefaultMacrosMenuItem.checked := true;
       LogOut(llDebug, _log_name_+': '+'File "%s" remains default macro file', [Config.DefaultMacrosFile]);
     end;
   end;
end;

procedure TMacroForm.DeleteMacroMenuItemClick(Sender: TObject);
begin
  macros.Delete(MacrosEditor.row - 1);
  UpdateGUI;
end;

procedure TMacroForm.EditMacroMenuItemClick(Sender: TObject);
var
  r, c: integer;
begin
  c := MacrosEditor.col;
  if c <> 1 then exit;
  r := MacrosEditor.row;
  if (r < 1) or (r > Config.ButtonCount) then exit;
  if macros[r-1].Kind = mkKbdMacro then
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
      if not macros[index].IsEmpty then
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
  EraseMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and (not macros[ndx].isEmpty);
  DeleteMacroMenuItem.Enabled := (ndx >= 0) and (ndx < Config.ButtonCount) and notEmpty;
  MoveMacroUpMenuItem.Enabled := (ndx > 0);
  MoveMacroDownMenuItem.Enabled := (ndx < Config.ButtonCount-1);
  ClearMacrosMenuItem.Enabled := not MacrosEmptyFrom(0);
end;

procedure TMacroForm.EraseMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
begin
  ndx := MacrosEditor.row - 1;
  if (ndx >= 0) and (ndx < Config.ButtonCount) then begin
    macros.erase(ndx);
    MacrosEditor.Cells[1, ndx+1] := '';
    MacrosEditor.Cells[2, ndx+1] := sMacroKind[pcCtrlV];
    LayoutForm.keys[ndx].Hint := '';
  end;
end;

function TMacroForm.FixupMacroName(const fn: string): string;
const
  _log_name_ = 'TMacroForm.FixupMacroName';
begin
  // assume fn = 'fname.macros.bak'
  result := ChangeFileext(fn, '');
   // avoid creating 'fname.macros.macros'
   while ExtractFileExt(result) = '.macros' do
     result := ChangeFileExt(result, '');
   result := ChangeFileext(result, '.macros');
   if result <> fn then
     LogOut(llDebug, _log_name_+': '+'Filename %s changed to %s', [fn, result]);
end;

procedure TMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  MacrosEditor.RowCount := config.ButtonCount+1;
  for i := 1 to config.ButtonCount do
    MacrosEditor.Cells[0,i] := KeyLabels[i];
  Macros.OnModified := @MacrosModified;
  UpdateGUI;
  if macros.filename = '' then
    SetMacrosFilename('<new>')
  else
    SetMacrosFilename(macros.filename);
end;

procedure TMacroForm.FormCreate(Sender: TObject);
var
  mk: TMacroKind;
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Width := (3*Width) div 2;
  Height := (3*Height) div 2;
  MacrosEditor.Columns[2].PickList.clear;
  for mk in TMacroKind do
    MacrosEditor.Columns[2].PickList.add(sMacroKind[mk]);
end;

procedure TMacroForm.FormHide(Sender: TObject);
begin
  MainForm.MacroDefItem.Checked := false;
end;

{ #todo 1 -oMichel -cImprovement : Verify before if deleting last macro is ok }
procedure TMacroForm.InsertMacroMenuItemClick(Sender: TObject);
var
  ndx: integer;
  j: integer;
begin
  ndx := MacrosEditor.row - 1;
  macros.insert(ndx);
  UpdateGUI;
end;

procedure TMacroForm.MacrosEditorEditingDone(Sender: TObject);
// not executed when editing kbdmacro
var
  r, c: integer;
  s: string;
  mk: TMacroKind;
begin
  r := MacrosEditor.Row;
  c := MacrosEditor.Col;
  s := MacrosEditor.Cells[c, r];
  if (r < 1) or (r > config.ButtonCount) then exit;
  dec(r);
  if c = 1 then begin
    if s = macros[r].Content then exit;
    macros[r].Content := s;
    LayoutForm.keys[r].Hint := s;
    if s = '' then begin
      macros[r].Kind := pcCtrlV;
      MacrosEditor.Cells[2,r+1] := sMacroKind[pcCtrlV];
    end;
    //Log(llDebug, 'Macro %d modified, layout form updated', [r]);
  end
  else if c = 2 then begin
    if s = sMacroKind[pcCtrlV] then
      mk := pcCtrlV
    else if s = sMacroKind[pcShiftInsert] then
      mk := pcShiftInsert
    else if s = sMacroKind[pcCustom] then
      mk := pcCustom
    else if s = sMacroKind[mkNone] then
      mk := mkNone
    else if s = sMacroKind[mkKbdMacro] then
      mk := mkKbdMacro
    else begin
      MacrosEditor.Cells[c, r+1] := sMacroKind[macros[r].Kind];
      exit;
    end;
    macros[r].Kind := mk;
    //LogForm.Log(llDebug, 'Paste command %d modified', [r]);
  end;
end;

procedure TMacroForm.MacrosEditorSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := aCol > 0;
  EditorPopMenu.AutoPopup := aCol = 1;
end;

procedure TMacroForm.MacrosEditorSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
const
  _log_name_ = 'TMacroForm.MacrosEditorSelectEditor';
begin
  // Grid editor must not be set before the grid has been filled by UpdateGUI
  if (aCol = 1) and (macros[ARow-1].Kind = mkKbdMacro) and guiUpdated then begin
    ProxyEditorButton.BoundsRect := Rect(10, 10, 8, 8); // make it invisible
    Editor := ProxyEditorButton;
    LogOut(llDebug, _log_name_+': '+'Proxy editor set for cell[%d, %d]', [aCol, aRow]);
  end;
end;

procedure TMacroForm.MacrosModified(Sender: TObject);
begin
  if macros.Modified then
    DefaultMacrosMenuItem.checked := false;
  ModifiedLabel.Caption := ifThen(macros.Modified, '*', '');
end;

procedure TMacroForm.MenuItem1Click(Sender: TObject);
begin
  if doEditPasteDelays then
    macros.modified := true;
end;

procedure TMacroForm.MoveMacroUpMenuItemClick(Sender: TObject);
var
  r: integer;
begin
  r := MacrosEditor.Row;
  if r <= 0 then exit;
  dec(r);
  macros.exchange(r, r-1);
  UpdateGUI;
  MacrosEditor.Row := r;
end;

procedure TMacroForm.MoveMacroDownMenuItemClick(Sender: TObject);
var
  r: integer;
begin
  r := MacrosEditor.Row;
  dec(r);
  if r >= Config.ButtonCount-1 then exit;
  macros.exchange(r,r+1);
  UpdateGUI;
  MacrosEditor.Row := r+2;
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
      macros.Load(filename);
      //LogForm.Log(llDebug, 'Loaded macro file "%s"', [filename]);
      fn := FixupMacroName(filename);
      SetMacrosFilename(fn);
      macros.modified := false;
      UpdateGUI;
      if (fn <> filename) and not FileExists(fn) then
        macros.Save(fn);
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
  EditKbdMacroForm.SetKbdMacro(macros[r-1].events); // = cells[c, r]
  LogOut(llDebug, 'Starting Keyboard macro editor');
  if EditKbdMacroForm.ShowModal = mrOk then begin
    macros[r-1].events := EditKbdMacroForm.GetKbdMacro;
    LayoutForm.keys[r-1].Hint := macros[r-1].content;
    MacrosEditor.cells[c, r] := macros[r-1].content;
  end;
  Application.QueueAsyncCall(@ProxyEditorRestoreSelection, 0);
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
    if macros.modified then begin
      x := left + 50;
      y := top + 50;
      mr := MessageDlgPos('Save the modified macro definitions before.',
        mtConfirmation, [mbYes, mbNo, mbCancel], 0, x, y);
      if (mr = mrCancel) then exit;
      if (mr = mrYes) then
         SaveMacroFileMenuItemClick(nil);
    end;
    macros.load(fn);
    SetMacrosFilename(fn);
    UpdateGUI;
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
      macros.Save(filename);
      SetMacrosFilename(filename);
      UpdateGUI;
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
    macros.Save(fn);
  end;
end;

function TMacroForm.SaveMacrosFile: integer;
var
  mr: TModalResult;
  x, y: integer;
begin
  result := mrOk;
  SaveMacroFileMenuItemClick(nil);
  if macros.modified then begin
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

procedure TMacroForm.UpdateGUI;
var
  i : integer;
begin
  for i := 0 to  config.ButtonCount-1 do begin
    MacrosEditor.Cells[1, i+1] := macros[i].content;
    MacrosEditor.Cells[2, i+1] := sMacroKind[macros[i].Kind];
  end;
  guiUpdated := true;
  LayoutForm.UpdateGUI;
end;

end.

