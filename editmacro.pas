unit editmacro;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, kbdev;

type

  { TEditKbdMacroForm }

  TEditKbdMacroForm = class(TForm)
    AddButton: TButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Button2: TButton;
    Button3: TButton;
    InsertButton: TButton;
    Label1: TLabel;
    ModifyButton: TButton;
    CheckBox1: TCheckBox;
    ModifiersCheckGroup: TCheckGroup;
    KeyNameComboBox: TComboBox;
    ImageList1: TImageList;
    Label3: TLabel;
    Label4: TLabel;
    MacroListBox: TListBox;
    KeyActionRadioGroup: TRadioGroup;
    TestSpeedButton: TSpeedButton;
    UpSpeedButton: TSpeedButton;
    DownSpeedButton: TSpeedButton;
    DeleteSpeedButton: TSpeedButton;
    ClearSpeedButton: TSpeedButton;
    procedure AddButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure ClearSpeedButtonClick(Sender: TObject);
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure DownSpeedButtonClick(Sender: TObject);
    procedure MacroListBoxClick(Sender: TObject);
    procedure MacroListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ModifyButtonClick(Sender: TObject);
    procedure TestSpeedButtonClick(Sender: TObject);
    procedure UpSpeedButtonClick(Sender: TObject);
  private
    Event: TKbdEvent;
    Macro: TKbdMacro;
    EventString: string;
    MacroString: string;
    procedure BuildEventString;
    procedure BuildMacroString;
    procedure UpdateSpeedEnables;
    procedure SetEvent(anEvent: TKbdEvent);
  public
    procedure SetMacro(const value: string);
    function GetMacroString: string;
  end;

var
  EditKbdMacroForm: TEditKbdMacroForm;

implementation

{$R *.lfm}

{ TEditKbdMacroForm }


procedure TEditKbdMacroForm.AddButtonClick(Sender: TObject);
var
  n: integer;
begin
  BuildEventString;
  n := length(macro);
  setlength(macro, n+1);
  macro[n] := event;
  MacroListBox.Items.add(EventString);
  if event.Press and checkbox1.checked then begin
    event.Press := false;
    n := length(macro);
    setlength(macro, n+1);
    macro[n] := event;
    MacroListBox.items.add(KbdEventToStr(Event));
    event.Press := true;
  end;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.Button2Click(Sender: TObject);
begin
  BuildMacroString;
  ModalResult := mrOk;
end;

procedure TEditKbdMacroForm.FormActivate(Sender: TObject);
var
  i: integer;
begin
  MacroListBox.items.beginUpdate;
  MacroListBox.items.clear;
  for i := 0 to length(macro)-1 do
    MacroListBox.items.add(KbdEventToStr(macro[i]));
  MacroListBox.items.endUpdate;
  if length(macro) > 0 then begin
    //SetEvent(macro[0]);
    MacroListBox.ItemIndex := 0;
  end
  else
    SetEvent(DefaultEvent);
end;

procedure TEditKbdMacroForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  KeyNameComboBox.Items.BeginUpdate;
  try
    KeyNameComboBox.Items.clear;
    for i := 0 to KEYCOUNT-1 do
      KeyNameComboBox.Items.add(KeyCodesAndStrings[i].Name);
  finally
    KeyNameComboBox.Items.BeginUpdate;
    KeyNameComboBox.ItemIndex := 107;
  end;
end;

procedure TEditKbdMacroForm.BuildEventString;
begin
  Event.VK := KeyCodesAndStrings[KeyNameComboBox.ItemIndex].code;
  Event.Press := KeyActionRadioGroup.ItemIndex = 0;
  Event.Shift := [];
  if ModifiersCheckGroup.Checked[0] then include(Event.Shift, ssShift);
  if ModifiersCheckGroup.Checked[1] then include(Event.Shift, ssCtrl);
  if ModifiersCheckGroup.Checked[2] then include(Event.Shift, ssAlt);
  EventString := KbdEventToStr(Event);
end;

procedure TEditKbdMacroForm.BuildMacroString;
  procedure addEvent(const s: string);
  begin
    if MacroString <> '' then
      MacroString := MacroString + ' ';
    MacroString := MacroString + '(' + s + ')';
  end;
var
  i: integer;
begin
  MacroString := '';
  for i := 0 to MacroListBox.Items.count-1 do
    addEvent(MacroListBox.Items[i]);
end;

procedure TEditKbdMacroForm.ClearSpeedButtonClick(Sender: TObject);
begin
  setlength(macro, 0);
  MacroListBox.Items.clear;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.DeleteSpeedButtonClick(Sender: TObject);
var
  ndx: integer;
  n, i: integer;
begin
  ndx := MacroListBox.ItemIndex;
  n := length(macro);
  if (ndx < 0) or (ndx >= n) then exit;
  for i := ndx to n-1 do
    macro[i] := macro[i+1];
  setlength(macro,n-1);
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
  if i >= length(macro) then exit;
  temp := macro[i+1];
  macro[i+1] := macro[i];
  macro[i] := temp;
  MacroListBox.Items.Exchange(i, i+1);
  MacroListBox.itemindex := i+1;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.InsertButtonClick(Sender: TObject);
var
  ndx: integer;
  n, i: integer;
begin
  BuildEventString;
  ndx := MacroListBox.ItemIndex;
  n := length(macro);
  if (ndx < 0) or (ndx >= n) then begin
    AddButtonClick(Sender);
    exit;
  end;
  setlength(macro, n+1);
  for i := n downto ndx+1 do
    macro[i] := macro[i-1];
  macro[ndx] := event;
  MacroListBox.items.Insert(ndx, EventString);

  if event.Press and checkbox1.checked then begin
    inc(n);
    inc(ndx);
    setlength(macro, n+1);
    for i := n downto ndx+1 do
      macro[i] := macro[i-1];
    event.Press := false;
    macro[ndx] := event;
    MacroListBox.items.Insert(ndx, KbdEventToStr(Event));
    event.Press := true;
  end;
  UpdateSpeedEnables;
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
  if (ndx < 0) or (ndx >= length(macro)) then exit;
  SetEvent(macro[ndx]);
end;

procedure TEditKbdMacroForm.ModifyButtonClick(Sender: TObject);
var
  ndx: integer;
  hasrelease: boolean;
begin
  ndx := MacroListBox.ItemIndex;
  if (ndx < 0) or (ndx >= length(macro)) then exit;
  hasrelease := (ndx < length(macro)-1) and (macro[ndx].VK = macro[ndx+1].VK)
   and (macro[ndx].shift = macro[ndx+1].shift);
  BuildEventString;
  MacroListBox.Items[ndx] := EventString;
  macro[ndx] := Event;
  if hasrelease and Event.Press then begin
    Event.Press := false;
    macro[ndx+1] := Event;
    MacroListBox.Items[ndx+1] :=  KbdEventToStr(Event);
    Event.Press := true;
  end;
end;

procedure TEditKbdMacroForm.TestSpeedButtonClick(Sender: TObject);
var
  s: string;
  n: integer;
begin
  n := testMacro(macro);
  if n = 0 then
    s := 'The macro is valid'
  else
    s := Format('The macro is not valid (error %d)', [n]);
  MessageDlg('Macro verification', s, mtInformation, [mbOK], 0);
end;

procedure TEditKbdMacroForm.UpSpeedButtonClick(Sender: TObject);
var
  temp: TKbdEvent;
  i: integer;
begin
  i := MacroListBox.itemindex;
  if i <= 0 then exit;
  temp := macro[i-1];
  macro[i-1] := macro[i];
  macro[i] := temp;
  MacroListBox.Items.Exchange(i-1, i);
  MacroListBox.itemindex := i-1;
  UpdateSpeedEnables;
end;

procedure TEditKbdMacroForm.SetEvent(anEvent: TKbdEvent);
var
  i: integer;
begin
  if KeyCodeFind(anEvent.VK, i) then begin
    Event := anEvent;
    KeyNameComboBox.ItemIndex := i;
    if Event.Press then KeyActionRadioGroup.ItemIndex := 0 else KeyActionRadioGroup.ItemIndex := 1;
    ModifiersCheckGroup.Checked[0] := ssShift In Event.Shift;
    ModifiersCheckGroup.Checked[1] := ssCtrl In Event.Shift;
    ModifiersCheckGroup.Checked[2] := ssAlt in Event.Shift;
  end;
end;

procedure TEditKbdMacroForm.SetMacro(const value: string);
begin
  setlength(macro, 0);
  macro := StrToMacro(value);
end;


function TEditKbdMacroForm.GetMacroString: string;
begin
  result := MacroString;
end;

procedure TEditKbdMacroForm.UpdateSpeedEnables;
var
  ndx, n: integer;
begin
  n := length(macro);
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

