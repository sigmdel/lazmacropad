unit custompastecommand;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, kbdev;

type

  { TCustomPasteForm }

  TCustomPasteForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    KeyNameComboBox: TComboBox;
    ShiftCheckBox: TCheckBox;
    CtrlCheckBox: TCheckBox;
    AltCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  CustomPasteForm: TCustomPasteForm;

function EditCustomPasteCommand(var VK: word; var shift: TKbdShift): boolean;

implementation

{$R *.lfm}

function EditCustomPasteCommand(var VK: word; var shift: TKbdShift): boolean;
var
  i: integer;
begin
  if not KeyCodeFind(VK, i) then
    i := 0;
  with CustomPasteForm do begin
    KeyNameComboBox.ItemIndex := i;
    ShiftCheckBox.Checked := ssShift In shift;
    CtrlCheckBox.Checked := ssCtrl In shift;
    AltCheckBox.Checked := ssAlt In shift;
    result := ShowModal = mrOk;
    shift := [];
    if result then begin
      VK := KeyCodesAndStrings[KeyNameComboBox.ItemIndex].code;
      if ShiftCheckBox.Checked then include(shift, ssShift);
      if CtrlCheckBox.Checked then include(shift, ssCtrl);
      if AltCheckBox.Checked then include(shift, ssAlt);
    end
    else
      VK := 0;
  end;
end;

{ TCustomPasteForm }

procedure TCustomPasteForm.FormCreate(Sender: TObject);
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


end.

