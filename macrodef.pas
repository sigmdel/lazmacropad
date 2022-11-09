unit macrodef;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Grids, LCLIntf, LMessages;

const
  LM_NEW_MACROFILENAME = LM_USER + 1;    // LM_USER = WM_USER
  LM_SAVE_MACROS_QUITTING = LM_NEW_MACROFILENAME + 1;

type

  { TMacroForm }

  TMacroForm = class(TForm)
    DefaultCheckbox: TCheckBox;
    Label1: TLabel;
    MacrosEditor: TStringGrid;
    MacrosFileNameEdit: TFileNameEdit;
    SaveDialog1: TSaveDialog;
    SaveMacrosButton: TButton;
    procedure DefaultCheckboxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure MacrosEditorEditingDone(Sender: TObject);
    procedure MacrosEditorSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure MacrosFileNameEditAcceptFileName(Sender: TObject;
      var Value: String);
    procedure MacrosFileNameEditEditingDone(Sender: TObject);
    procedure MacrosFileNameEditEnter(Sender: TObject);
    procedure SaveMacrosButtonClick(Sender: TObject);
  private
    currentmacrosfile: string;
    procedure UpdateGUI;
    procedure SetMacrosModified(value: boolean);
  public
    procedure NewMacroFileName(var Msg: TLMessage); message LM_NEW_MACROFILENAME;
    procedure SaveMacrosQuitting(var Msg: TLMessage); message LM_SAVE_MACROS_QUITTING;
    procedure SaveMacrosBeforeQuitting;
  end;

var
  MacroForm: TMacroForm;


implementation

{$R *.lfm}

uses
  main, params, keymap;

resourcestring
  SpcCtrlV = 'Ctrl+V';
  SpcShiftInsert = 'Shift+Insert';

var
  sPasteCommands : array[TPasteCommand] of string = (SpcCtrlV, SPcShiftInsert, '');

{ TMacroForm }

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

procedure TMacroForm.DefaultCheckboxChange(Sender: TObject);
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

procedure TMacroForm.MacrosFileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  PostMessage(self.handle, LM_NEW_MACROFILENAME, 0, 0);
  // use post to return immediately so that MacrosFilenameEdit.Filename
  // get set to Value before MacrosFileNameEditEditingDone is
  // called by the message handler NewMacroFileName(var Msg: TLMessage)
end;

procedure TMacroForm.MacrosFileNameEditEditingDone(Sender: TObject);
begin
  if MacrosFileNameEdit.filename <> currentMacrosFile then begin
     LoadMacros(MacrosFileNameEdit.filename);
     currentMacrosFile := MacrosFileNameEdit.filename;
     UpdateGUI;
   end;
end;

procedure TMacroForm.MacrosFileNameEditEnter(Sender: TObject);
begin
  currentmacrosfile := MacrosFileNameEdit.filename;
end;

procedure TMacroForm.NewMacroFileName(var Msg: TLMessage);
begin
  MacrosFileNameEditEditingDone(nil);
end;

procedure TMacroForm.SaveMacrosBeforeQuitting;
begin
  PostMessage(self.handle, LM_SAVE_MACROS_QUITTING, 0, 0);
end;

procedure TMacroForm.SaveMacrosButtonClick(Sender: TObject);
begin
  with SaveDialog1 do begin
    filename := MacrosFileNameEdit.filename;
    if execute then begin
      filename := ChangeFileext(filename, '.macros');
      SaveMacros(filename);
      MacrosFileNameEdit.filename := filename;
      SetMacrosModified(false);
      UpdateGUI;
    end;
  end;
end;

procedure TMacroForm.SaveMacrosQuitting(var Msg: TLMessage);
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
    SetMacrosmodified(false);
  end;
  MainForm.close;
end;

procedure TMacroForm.SetMacrosModified(value: boolean);
begin
  macrosmodified := value;
  SaveMacrosButton.Enabled := value;
end;

procedure TMacroForm.UpdateGUI;
var
  i : integer;
begin
  for i := 0 to  config.ButtonCount-1 do begin
    MacrosEditor.Cells[1, i+1] := macros[i];
    MacrosEditor.Cells[2, i+1] := sPasteCommands[pastes[i]];
  end;
  SaveMacrosButton.Enabled := macrosmodified;
  DefaultCheckbox.checked := (Config.DefaultMacrosFile <> '')
    and (MacrosFileNameEdit.Filename = Config.DefaultMacrosFile);
  LayoutForm.UpdateGUI;
end;




end.

