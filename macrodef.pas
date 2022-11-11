unit macrodef;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, LCLIntf, LMessages, Buttons, Menus, ExtCtrls, StrUtils;

const
  LM_SAVE_MACROS_QUITTING = LM_USER + 1;

type

  { TMacroForm }

  TMacroForm = class(TForm)
    DefaultMacrosMenuItem: TMenuItem;
    ModifiedLabel: TLabel;
    MacrosFilenameLabel: TLabel;
    OpenDialog1: TOpenDialog;
    Separator1: TMenuItem;
    SaveMacroFileMenuItem: TMenuItem;
    OpenMacroFileMenuItem: TMenuItem;
    PopMacroMenuButton: TButton;
    MacrosLabel: TLabel;
    MacrosEditor: TStringGrid;
    MacrosPopupMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure DefaultMacrosMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure MacrosEditorEditingDone(Sender: TObject);
    procedure MacrosEditorSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OpenMacroFileMenuItemClick(Sender: TObject);
    procedure PopMacroMenuButtonClick(Sender: TObject);
    procedure SaveMacroFileMenuItemClick(Sender: TObject);
  private
    procedure UpdateGUI;
    procedure SetMacrosModified(value: boolean);
  public
    procedure SaveMacrosQuitting(var Msg: TLMessage); message LM_SAVE_MACROS_QUITTING;
    procedure SaveMacrosBeforeQuitting;
    procedure SetMacrosFilename(const value: string);
  end;

var
  MacroForm: TMacroForm;


implementation

{$R *.lfm}

uses
  main, params, keymap;

resourcestring
  SpcCtrlV = 'Ctrl+V';                 // 'Ctrl+V'
  SpcShiftInsert = 'Shift+Insert';     // 'Maj+Inser

var
  sPasteCommands : array[TPasteCommand] of string = (SpcCtrlV, SPcShiftInsert, '');

{ TMacroForm }

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
begin
  with OpenDialog1 do begin
    filename := MacrosFilenameLabel.hint;
    if (filename = '') or (filename[1] = '<') then begin
      filename := '';
      initialdir := configdir;
    end;
    if execute then begin
      filename := ChangeFileext(filename, '.macros');
      LoadMacros(filename);
      SetMacrosFilename(filename);
      SetMacrosModified(false);
      UpdateGUI;
    end;
  end;
end;

procedure TMacroForm.PopMacroMenuButtonClick(Sender: TObject);
begin
  MacrosPopupMenu.PopUp;
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

