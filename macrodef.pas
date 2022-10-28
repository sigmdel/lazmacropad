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
    Label3: TLabel;
    MacrosEditor: TStringGrid;
    MacrosFileNameEdit: TFileNameEdit;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
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
    procedure PasteCommandButtonsChange(Sender: TObject);
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

{ TMacroForm }

procedure TMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to BUTTON_COUNT-1 do
    MacrosEditor.Cells[0,i+1] := inttohex(i, 1);
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
begin
  if (MacrosEditor.Row >= 1) and (MacrosEditor.Row <= BUTTON_COUNT) and
  (macros[MacrosEditor.Row-1] <> MacrosEditor.Cells[1, MacrosEditor.Row]) then begin
     macros[MacrosEditor.Row-1] := MacrosEditor.Cells[1, MacrosEditor.Row];
     SetMacrosModified(true);
     LayoutForm.keys[MacrosEditor.Row-1].Hint := macros[MacrosEditor.Row-1];
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

procedure TMacroForm.PasteCommandButtonsChange(Sender: TObject);
var
  newpaste: TPasteCommand;
begin
  if RadioButton5.checked then
    newpaste := pcCtrlV
  else
    newpaste := pcShiftInsert;
  if PasteCommand <> newpaste then begin
    PasteCommand := newpaste;
    SetMacrosModified(true);
  end;
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
  for i := 0 to  BUTTON_COUNT-1 do
    MacrosEditor.Cells[1, i+1] := macros[i];
  SaveMacrosButton.Enabled := macrosmodified;
  DefaultCheckbox.checked := (Config.DefaultMacrosFile <> '')
    and (MacrosFileNameEdit.Filename = Config.DefaultMacrosFile);
  RadioButton5.checked := PasteCommand = pcCtrlV;
  RadioButton6.checked := PasteCommand = pcShiftInsert;
  LayoutForm.UpdateGUI;
end;




end.

