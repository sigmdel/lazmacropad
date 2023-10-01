unit editKbdMacro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, Spin, ukbdev;

type

  { TEditKbdMacroForm }

  TEditKbdMacroForm = class(TForm)
    AddButton: TButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    AcceptButton: TButton;
    CancelButton: TButton;
    InsertButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ModifyButton: TButton;
    AutoRelaseCheckBox: TCheckBox;
    ModifiersCheckGroup: TCheckGroup;
    KeyNameComboBox: TComboBox;
    ImageList1: TImageList;
    Label3: TLabel;
    Label4: TLabel;
    MacroListBox: TListBox;
    KeyActionRadioGroup: TRadioGroup;
    DelaySpinEdit: TSpinEdit;
    TestSpeedButton: TSpeedButton;
    UpSpeedButton: TSpeedButton;
    DownSpeedButton: TSpeedButton;
    DeleteSpeedButton: TSpeedButton;
    ClearSpeedButton: TSpeedButton;
    procedure AddButtonClick(Sender: TObject);
    procedure AcceptButtonClick(Sender: TObject);
    procedure DelaySpinEditChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure ClearSpeedButtonClick(Sender: TObject);
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure DownSpeedButtonClick(Sender: TObject);
    procedure KeyActionRadioGroupClick(Sender: TObject);
    procedure KeyNameComboBoxChange(Sender: TObject);
    procedure MacroListBoxClick(Sender: TObject);
    procedure MacroListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ModifiersCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure ModifyButtonClick(Sender: TObject);
    procedure TestSpeedButtonClick(Sender: TObject);
    procedure UpSpeedButtonClick(Sender: TObject);
  private
    FKbdEvent: TKbdEvent;  // currently edited TKbdEvent
    FKbdMacro: TKbdMacro;  // currently edited TKbeMacro
    procedure UpdateSpeedEnables; // updated all left speed buttons enable property
    procedure SetEvent(anEvent: TKbdEvent); // FKbdEvent := anEvent and updates displayed properties
  public
    procedure SetKbdMacro(value: TKbdMacro);
    function GetKbdMacro: TKbdMacro;
  end;

var
  EditKbdMacroForm: TEditKbdMacroForm;

implementation

{$R *.lfm}

{ TEditKbdMacroForm }

procedure TEditKbdMacroForm.AcceptButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditKbdMacroForm.AddButtonClick(Sender: TObject);
var
  n: integer;
begin
  n := length(FKbdMacro);
  setlength(FKbdMacro, n+1);
  FKbdMacro[n] := FKbdEvent;
  MacroListBox.Items.add(FKbdEvent.EventToStr);
  MacroListBox.ItemIndex := n;
  if FKbdEvent.Press and AutoRelaseCheckBox.checked then begin
    // add a corresponding key release event
    FKbdEvent.Press := false;
    n := length(FKbdMacro);
    setlength(FKbdMacro, n+1);
    FKbdMacro[n] := FKbdEvent;
    MacroListBox.items.add(FKbdEvent.EventToStr);
    FKbdEvent.Press := true;
  end;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.ClearSpeedButtonClick(Sender: TObject);
begin
  setlength(FKbdMacro, 0);
  MacroListBox.Items.clear;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.DelaySpinEditChange(Sender: TObject);
begin
  FKbdEvent.Delayms := DelaySpinEdit.Value;
end;

procedure TEditKbdMacroForm.DeleteSpeedButtonClick(Sender: TObject);
var
  ndx: integer;
  n, i: integer;
begin
  ndx := MacroListBox.ItemIndex;
  n := length(FKbdMacro);
  if (ndx < 0) or (ndx >= n) then exit;
  for i := ndx to n-2 do
    FKbdMacro[i] := FKbdMacro[i+1];
  setlength(FKbdMacro,n-1);
  MacroListBox.items.Delete(ndx);
  if ndx >= MacroListBox.Items.count then
    ndx := MacroListBox.Items.count-1;
  MacroListBox.ItemIndex := ndx;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.DownSpeedButtonClick(Sender: TObject);
var
  temp: TKbdEvent;
  i: integer;
begin
  i := MacroListBox.itemindex;
  if i >= length(FKbdMacro) then exit;
  temp := FKbdMacro[i+1];
  FKbdMacro[i+1] := FKbdMacro[i];
  FKbdMacro[i] := temp;
  MacroListBox.Items.Exchange(i, i+1);
  MacroListBox.itemindex := i+1;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  MacroListBox.items.beginUpdate;
  MacroListBox.items.clear;
  for i := 0 to length(FKbdMacro)-1 do
    MacroListBox.items.add(FKbdMacro[i].EventToStr);
  MacroListBox.items.endUpdate;
  if length(FKbdMacro) > 0 then begin
    //SetEvent(FKbdMacro[0]);
    MacroListBox.ItemIndex := 0;
  end
  else begin
    with FKbdEvent do begin
      Code := $7C; // VK_F13;
      Press := true;
      Shift := [];
      DelayMS := 0;
    end;
    SetEvent(FKbdEvent);
  end;
end;

procedure TEditKbdMacroForm.FormCreate(Sender: TObject);
begin
  AssignKeyNamesCodes(KeyNameComboBox.Items);
end;

procedure TEditKbdMacroForm.FormDestroy(Sender: TObject);
begin
  KeynameComboBox.Items.Clear;
end;

function TEditKbdMacroForm.GetKbdMacro: TKbdMacro;
begin
  result := FKbdMacro;
end;

procedure TEditKbdMacroForm.InsertButtonClick(Sender: TObject);
var
  ndx: integer;
  n, i: integer;
begin
  ndx := MacroListBox.ItemIndex;
  n := length(FKbdMacro);
  if (ndx < 0) or (ndx >= n) then begin
    AddButtonClick(Sender);
    exit;
  end;
  setlength(FKbdMacro, n+1);
  for i := n downto ndx+1 do
    FKbdMacro[i] := FKbdMacro[i-1];
  FKbdMacro[ndx] := FKbdEvent;
  MacroListBox.items.Insert(ndx, FkbdEvent.EventToStr);
  MacroListBox.ItemIndex := ndx;
  if FKbdEvent.Press and AutoRelaseCheckBox.checked then begin
    inc(n);
    inc(ndx);
    setlength(FKbdMacro, n+1);
    for i := n downto ndx+1 do
      FKbdMacro[i] := FKbdMacro[i-1];
    FKbdEvent.Press := false;
    FKbdMacro[ndx] := FKbdEvent;
    MacroListBox.items.Insert(ndx, FKbdEvent.EventToStr);
    FKbdEvent.Press := true;
  end;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.KeyActionRadioGroupClick(Sender: TObject);
begin
  FKbdEvent.Press := KeyActionRadioGroup.ItemIndex = 0;
end;

procedure TEditKbdMacroForm.KeyNameComboBoxChange(Sender: TObject);
begin
  if KeyNameComboBox.ItemIndex < 0 then exit;
  FKbdEvent.Code := PtrUint(Pointer(KeyNameComboBox.Items.Objects[KeyNameComboBox.ItemIndex])) and $FF;
end;

procedure TEditKbdMacroForm.MacroListBoxClick(Sender: TObject);
begin
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.MacroListBoxSelectionChange(Sender: TObject; User: boolean);
var
  ndx: integer;
begin
  ndx := MacroListBox.ItemIndex;
  if (ndx < 0) or (ndx >= length(FKbdMacro)) then exit;
  SetEvent(FKbdMacro[ndx]);
end;

procedure TEditKbdMacroForm.ModifiersCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  FKbdEvent.Shift := [];
  if ModifiersCheckGroup.Checked[0] then include(FKbdEvent.Shift, ksShift);
  if ModifiersCheckGroup.Checked[1] then include(FKbdEvent.Shift, ksCtrl);
  if ModifiersCheckGroup.Checked[2] then include(FKbdEvent.Shift, ksAlt);
  if ModifiersCheckGroup.Checked[3] then include(FKbdEvent.Shift, ksAltGr);
end;

procedure TEditKbdMacroForm.ModifyButtonClick(Sender: TObject);
var
  ndx: integer;
  hasrelease: boolean;
begin
  ndx := MacroListBox.ItemIndex;
  if (ndx < 0) or (ndx >= length(FKbdMacro)) then exit;
  hasrelease := (ndx < length(FKbdMacro)-1) and (FKbdMacro[ndx].Code = FKbdMacro[ndx+1].Code)
   and (FKbdMacro[ndx].shift = FKbdMacro[ndx+1].shift);
  FKbdMacro[ndx] := FKbdEvent;
  MacroListBox.Items[ndx] := FKbdEvent.EventtoStr;
  if hasrelease and FKbdEvent.Press then begin
    FKbdEvent.Press := false;
    FKbdMacro[ndx+1] := FKbdEvent;
    MacroListBox.Items[ndx+1] :=  FKbdEvent.EventToStr;
    FKbdEvent.Press := true;
  end;
end;

procedure TEditKbdMacroForm.SetEvent(anEvent: TKbdEvent);
var
  i: integer;
begin
  if KeyCodeFind(anEvent.Code, i) then begin
    FKbdEvent := anEvent;
    KeyNameComboBox.ItemIndex := i;
    if FKbdEvent.Press then KeyActionRadioGroup.ItemIndex := 0 else KeyActionRadioGroup.ItemIndex := 1;
    ModifiersCheckGroup.Checked[0] := ksShift In FKbdEvent.Shift;
    ModifiersCheckGroup.Checked[1] := ksCtrl In FKbdEvent.Shift;
    ModifiersCheckGroup.Checked[2] := ksAlt in FKbdEvent.Shift;
    ModifiersCheckGroup.Checked[3] := ksAltGr in FKbdEvent.Shift;
    DelaySpinEdit.Value := FKbdEvent.delayms;
  end;
end;

procedure TEditKbdMacroForm.SetKbdMacro(value: TKbdMacro);
begin
  setlength(FKbdMacro, 0);
  FKbdMacro := value;
end;

procedure TEditKbdMacroForm.TestSpeedButtonClick(Sender: TObject);
var
  s: string;
  n: integer;
  i: integer;
begin
  i := 0;
  n := testKbdMacro(FKbdMacro, i);
  if n = 0 then
    s := 'The macro is valid'
  else
    s := Format('The macro is not valid (error %d) in Event %d', [n, i]);
  MessageDlg('Macro verification', s, mtInformation, [mbOK], 0);
end;

procedure TEditKbdMacroForm.UpSpeedButtonClick(Sender: TObject);
var
  temp: TKbdEvent;
  i: integer;
begin
  i := MacroListBox.itemindex;
  if i <= 0 then exit;
  temp := FKbdMacro[i-1];
  FKbdMacro[i-1] := FKbdMacro[i];
  FKbdMacro[i] := temp;
  MacroListBox.Items.Exchange(i-1, i);
  MacroListBox.itemindex := i-1;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.UpdateSpeedEnables;
var
  ndx, n: integer;
begin
  n := length(FKbdMacro);
  ndx := MacroListBox.ItemIndex;
  if n < 1 then begin
    ModifyButton.Enabled := false;
    UpSpeedButton.Enabled := false;
    DownSpeedButton.Enabled := false;
    DeleteSpeedButton.Enabled := false;
    ClearSpeedButton.Enabled := false;
    TestSpeedButton.Enabled := false;
  end
  else begin
    ModifyButton.Enabled := (ndx >= 0) and (ndx < n);
    UpSpeedButton.Enabled := ndx > 0;
    DownSpeedButton.Enabled := (ndx >= 0) and (ndx < n-1);
    DeleteSpeedButton.Enabled := ndx >= 0;
    ClearSpeedButton.Enabled := true;
    TestSpeedButton.Enabled := true;
  end;
end;

end.

